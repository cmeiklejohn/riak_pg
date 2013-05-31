%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Subscribe vnode.

-module(riak_pubsub_subscriptions_vnode).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(riak_core_vnode).

-include_lib("riak_pubsub.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-export([subscribe/4,
         unsubscribe/4]).

-export([repair/3]).

-record(state, {partition, channels}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state{partition=Partition, channels=dict:new()}}.

%% @doc Subscribe to a channel.
subscribe(Preflist, Identity, Channel, Pid) ->
    riak_core_vnode_master:command(Preflist,
                                   {subscribe, Identity, Channel, Pid},
                                   {fsm, undefined, self()},
                                   riak_pubsub_subscriptions_vnode_master).

%% @doc Unsubscribe to a channel.
unsubscribe(Preflist, Identity, Channel, Pid) ->
    riak_core_vnode_master:command(Preflist,
                                   {unsubscribe, Identity, Channel, Pid},
                                   {fsm, undefined, self()},
                                   riak_pubsub_subscriptions_vnode_master).

%% @doc Perform repair.
repair(IndexNode, Channel, Pids) ->
    riak_core_vnode_master:command(IndexNode,
                                   {repair, Channel, Pids},
                                   ignore,
                                   riak_pubsub_subscriptions_vnode_master).

%% @doc Perform subscription as part of repair.
handle_command({repair, Channel, Pids},
               _Sender,
               #state{channels=Channels0, partition=Partition}=State) ->
    lager:warning("Received repair for ~p and ~p and ~p.\n",
                  [Channel, Pids, Partition]),

    %% Generate key for gproc.
    Key = riak_pubsub_gproc:key(Channel, Partition),

    %% Store back into the dict.
    Channels = dict:store(Channel, Pids, Channels0),

    %% Save to gproc.
    ok = riak_pubsub_gproc:store(Key, Pids),

    {noreply, State#state{channels=Channels}};

%% @doc Respond to a subscription request.
handle_command({subscribe, {ReqId, _}, Channel, Pid},
               _Sender,
               #state{channels=Channels0, partition=Partition}=State) ->
    lager:warning("Received subscribe for ~p and ~p and ~p.\n",
                  [Channel, Pid, Partition]),

    %% Generate key for gproc.
    Key = riak_pubsub_gproc:key(Channel, Partition),

    %% Find existing list of Pids, and add object to it.
    Pids0 = pids(Channels0, Channel, riak_dt_orset:new()),
    Pids = riak_dt_orset:update({add, Pid}, Partition, Pids0),

    %% Store back into the dict.
    Channels = dict:store(Channel, Pids, Channels0),

    %% Save to gproc.
    ok = riak_pubsub_gproc:store(Key, Pids),

    %% Return updated channels.
    {reply, {ok, ReqId}, State#state{channels=Channels}};

%% @doc Respond to a unsubscription request.
handle_command({unsubscribe, {ReqId, _}, Channel, Pid},
               _Sender,
               #state{channels=Channels0, partition=Partition}=State) ->
    lager:warning("Received unsubscribe for ~p and ~p and ~p.\n",
                  [Channel, Pid, Partition]),

    %% Generate key for gproc.
    Key = riak_pubsub_gproc:key(Channel, Partition),

    %% Find existing list of Pids, and add object to it.
    Pids0 = pids(Channels0, Channel, riak_dt_orset:new()),
    Pids = riak_dt_orset:update({remove, Pid}, Partition, Pids0),

    %% Store back into the dict.
    Channels = dict:store(Channel, Pids, Channels0),

    %% Save to gproc.
    ok = riak_pubsub_gproc:store(Key, Pids),

    {reply, {ok, ReqId}, State#state{channels=Channels}};

%% @doc Default handler.
handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

%% @doc Fold over the dict for handoff.
handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Acc = dict:fold(Fun, Acc0, State#state.channels),
    {reply, Acc, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

%% @doc Handle receiving data from handoff.  Decode data and
%%      perform subscriptions.
handle_handoff_data(Data,
                    #state{channels=Channels0, partition=Partition}=State) ->
    {Channel, Pids} = binary_to_term(Data),

    %% Generate key for gproc.
    Key = riak_pubsub_gproc:key(Channel, Partition),

    %% Find existing list of Pids, and add object to it.
    Pids0 = pids(Channels0, Channel, riak_dt_orset:new()),
    MPids = riak_dt_orset:merge(Pids, Pids0),

    %% Store back into the dict.
    Channels = dict:store(Channel, MPids, Channels0),

    %% Save to gproc.
    ok = riak_pubsub_gproc:store(Key, MPids),

    {reply, ok, State#state{channels=Channels}}.

encode_handoff_item(Channel, Pids) ->
    term_to_binary({Channel, Pids}).

is_empty(#state{channels=Channels}=State) ->
    case dict:size(Channels) of
        0 ->
            {true, State};
        _ ->
            {false, State}
    end.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Return pids from the dict.
-spec pids(dict(), atom(), term()) -> term().
pids(Channels, Channel, Default) ->
    case dict:find(Channel, Channels) of
        {ok, Object} ->
            Object;
        _ ->
            Default
    end.
