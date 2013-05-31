%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Publish vnode.

-module(riak_pubsub_publish_vnode).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(riak_core_vnode).

-include_lib("riak_pubsub.hrl").

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

-export([publish/4]).

-record(state, {partition, node}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state{partition=Partition, node=node()}}.

%% @doc Publish a message.
publish(Preflist, Identity, Channel, Message) ->
    riak_core_vnode_master:command(
        Preflist,
        {publish, Identity, Channel, Message},
        {fsm, undefined, self()},
        riak_pubsub_publish_vnode_master).

%% @doc When receiving a message, find all globally
%%      registered listeners for the message and perform the relay.
handle_command({publish, {ReqId, _}, Channel, _Message},
               _Sender,
               #state{partition=Partition, node=Node}=State) ->
    Reply = try
        Key = {p, l, {riak_pubsub_subscription, Channel, Partition}},
        [{_Pid, Pids}] = gproc:lookup_values(Key),
        Pids
    catch
        _:_ ->
            []
    end,

    {reply, {ok, ReqId, {Partition, Node}, Reply}, State};

%% @doc Default handler.
handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
