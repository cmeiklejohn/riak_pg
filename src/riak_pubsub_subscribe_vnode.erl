%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Subscribe vnode.

-module(riak_pubsub_subscribe_vnode).
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

-export([subscribe/4]).

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
                                   riak_pubsub_subscribe_vnode_master).


%% @doc Respond to a subscription; launch a child process,
%%      and register it with gproc under a given channel name.
handle_command({subscribe, {ReqId, _}, Channel, Pid},
               _Sender,
               #state{channels=Channels0}=State) ->
    lager:warning("Received subscribe for ~p and ~p.\n",
                  [Channel, Pid]),

    case subscribe(Channels0, Channel, Pid) of
        {error, Error} ->
            {reply, {ok, ReqId, {error, Error}}, State};
        {ok, Channels} ->
            {reply, {ok, ReqId}, State#state{channels=Channels}}
    end;
handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Acc = dict:fold(Fun, Acc0, State#state.channels),
    {reply, Acc, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(Data, #state{channels=Channels0}=State) ->
    {Channel, Pids} = binary_to_term(Data),

    case subscribe(Channels0, Channel, Pids) of
        {error, Error} ->
            {reply, {error, Error}, State};
        {ok, Channels} ->
            {reply, ok, State#state{channels=Channels}}
    end.

encode_handoff_item(Channel, Pids) ->
    term_to_binary({Channel, Pids}).

is_empty(#state{channels=Channels}=State) ->
    case dict:size(Channels) of
        0 -> {true, State};
        _ -> {false, State}
    end.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% @doc Subscribe to a channel with a given pid, and return an
%%      updated dict of channel to pid mappings.
-spec subscribe(dict(), term(), pid() | list(pid())) -> dict().
subscribe(Channels0, Channel, Pid) when is_pid(Pid) ->
    case riak_pubsub_subscription_sup:start_child(Channel, Pid) of
        {error, Error} ->
            {error, Error};
        _ ->
            Channels = try
                            dict:append_list(Channel, [Pid], Channels0)
                       catch
                            _:_ ->
                                dict:store(Channel, [Pid], Channels0)
                       end,
            {ok, Channels}
    end;
subscribe(Channels0, Channel, Pids0) when is_list(Pids0) ->
    lists:foldl(fun(Pid, Channels) ->
                subscribe(Channels, Channel, Pid) end, Channels0, Pids0).
