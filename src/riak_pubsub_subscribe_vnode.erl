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
               #state{channels=Channels0, partition=Partition}=State) ->
    lager:warning("Received subscribe for ~p and ~p and ~p.\n",
                  [Channel, Pid, Partition]),

    case perform(Channels0, Partition, Channel, Pid) of
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

handle_handoff_data(Data, #state{channels=Channels0, partition=Partition}=State) ->
    {Channel, Pids} = binary_to_term(Data),

    case perform(Channels0, Partition, Channel, Pids) of
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
perform(Channels0, Partition, Channel, Pid) when is_pid(Pid) ->
    lager:warning("Starting subscription for ~p and ~p.\n",
                  [Channel, Pid]),

    Key = {p, l, {riak_pubsub_subscription, Channel, Partition}},

    %% Attempt to register the key if it hasn't been yet.
    try
        gproc:reg(Key)
    catch
        _:_ ->
            ok
    end,

    %% Spawn processes if necessary.
    case already_spawned(Channels0, Channel, Pid) of
        false ->
            try
                Channels = update_channels(Channels0, Pid, Channel),

                %% Get updated list of mappings.
                {ok, Pids} = dict:find(Channel, Channels),

                %% Update gproc with the new set of pids.
                true = gproc:set_value(Key, Pids),

                {ok, Channels}
            catch
                _:_ ->
                    {error, registration_failed}
            end;
        true ->
            {ok, Channels0}
    end;
perform(Channels0, Partition, Channel, Pids0) when is_list(Pids0) ->
    Channels1 = lists:foldl(fun(Pid, Channels) ->
                case perform(Channels, Partition, Channel, Pid) of
                    {error, _} ->
                        Channels;
                    {ok, Channels1} ->
                        Channels1
                end
        end, Channels0, Pids0),
    {ok, Channels1}.

%% @doc Determine if we've already spawned a process for this.
already_spawned(Channels, Channel, Pid) ->
    case dict:find(Channel, Channels) of
        {ok, Pids} ->
            lists:member(Pid, Pids);
        _ ->
            false
    end.

%% @doc Update channels listing with new channel/pid mapping.
update_channels(Channels, Pid, Channel) ->
    try
         dict:append_list(Channel, [Pid], Channels)
    catch
         _:_ ->
             dict:store(Channel, [Pid], Channels)
    end.
