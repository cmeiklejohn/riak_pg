%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Unsubscribe FSM.

-module(riak_pubsub_unsubscribe_fsm).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(gen_fsm).

-include_lib("riak_pubsub.hrl").

%% API
-export([start_link/4,
         unsubscribe/2]).

%% Callbacks
-export([init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3]).

%% States
-export([prepare/2,
         execute/2,
         waiting/2]).

-record(state, {preflist,
                req_id,
                coordinator,
                from,
                channel,
                pid,
                responses}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReqId, From, Channel, Pid) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Channel, Pid], []).

unsubscribe(Channel, Pid) ->
    ReqId = riak_pubsub:mk_reqid(),
    riak_pubsub_unsubscribe_fsm_sup:start_child(
        [ReqId, self(), Channel, Pid]),
    {ok, ReqId}.

%%%===================================================================
%%% Callbacks
%%%===================================================================

handle_info(_Info, _StateName, StateData) ->
    {stop, badmsg, StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop, badmsg, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, badmsg, StateData}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    ok.

%%%===================================================================
%%% States
%%%===================================================================

%% @doc Initialize the request.
init([ReqId, From, Channel, Pid]) ->
    State = #state{preflist=undefined,
                   req_id=ReqId,
                   coordinator=node(),
                   from=From,
                   channel=Channel,
                   pid=Pid,
                   responses=0},
    {ok, prepare, State, 0}.

%% @doc Prepare request by retrieving the preflist.
prepare(timeout, #state{channel=Channel}=State) ->
    DocIdx = riak_core_util:chash_key({<<"subscriptions">>, Channel}),
    Preflist = riak_core_apl:get_primary_apl(DocIdx, ?N,
                                             riak_pubsub_subscriptions),
    Preflist2 = [{Index, Node} || {{Index, Node}, _Type} <- Preflist],
    {next_state, execute, State#state{preflist=Preflist2}, 0}.

%% @doc Execute the request.
execute(timeout, #state{preflist=Preflist,
                        req_id=ReqId,
                        coordinator=Coordinator,
                        channel=Channel,
                        pid=Pid}=State) ->
    riak_pubsub_subscriptions_vnode:unsubscribe(Preflist,
                                                {ReqId, Coordinator},
                                                Channel, Pid),
    {next_state, waiting, State}.

%% @doc Attempt to write to every single node responsible for this
%%      channel.
waiting({ok, ReqId}, #state{responses=Responses0, from=From}=State0) ->
    Responses = Responses0 + 1,
    State = State0#state{responses=Responses},
    case Responses =:= ?W of
        true ->
            From ! {ReqId, ok},
            {stop, normal, State};
        false ->
            {next_state, waiting, State}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
