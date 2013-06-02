%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Message proxy vnode.

-module(riak_pg_message_proxy_vnode).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(riak_core_vnode).

-include_lib("riak_pg.hrl").
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

-export([accept/3]).

-record(state, {partition, messages}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state{partition=Partition, messages=dict:new()}}.

%% @doc Accept a message and attempt to deliver it to the vnodes
%%      defined in the preflist, in order.
accept([{Index, Node}|Preflist], Message, Pid) ->
    try
        riak_core_vnode_master:sync_command({Index, Node},
                                            {accept, Message, Pid},
                                            riak_pg_message_proxy_vnode_master),
        ok
    catch
        _:_ ->
            accept(Preflist, Message, Pid)
    end;
accept([], _Message, _Pid) ->
    {error, preflist_exhausted}.

%% @doc Respond to a message.
handle_command({accept, Message, Pid},
               _Sender, #state{messages=Messages0}=State) ->
    Node = node(Pid),
    Messages = case net_adm:ping(Node) of
        pong ->
            Pid ! Message,
            Messages0;
        pang ->
            ReqId = riak_pg:mk_reqid(),
            dict:store(ReqId, {Message, Pid}, Messages0)
    end,
    {reply, ok, State#state{messages=Messages}};

%% @doc Default handler.
handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

%% @doc Fold over the dict for handoff.
handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Acc = dict:fold(Fun, Acc0, State#state.messages),
    {reply, Acc, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

%% @doc Handle receiving handoff data.
handle_handoff_data(Data, #state{messages=Messages0}=State) ->
    {ReqId, {Message, Pid}} = binary_to_term(Data),
    Node = node(Pid),
    Messages = case net_adm:ping(Node) of
        pong ->
            Pid ! Message,
            Messages0;
        pang ->
            ReqId = riak_pg:mk_reqid(),
            dict:store(ReqId, {Message, Pid}, Messages0)
    end,
    {reply, ok, State#state{messages=Messages}}.

encode_handoff_item(ReqId, Item) ->
    term_to_binary({ReqId, Item}).

is_empty(#state{messages=Messages}=State) ->
    case dict:size(Messages) of
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
