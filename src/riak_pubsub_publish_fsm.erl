%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Publish FSM.

-module(riak_pubsub_publish_fsm).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(gen_fsm).

-include_lib("riak_pubsub.hrl").

%% API
-export([start_link/4,
         publish/2]).

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
                message,
                num_responses,
                pid_mappings}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReqId, From, Channel, Message) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Channel, Message], []).

publish(Channel, Message) ->
    ReqId = mk_reqid(),
    riak_pubsub_publish_fsm_sup:start_child(
        [ReqId, self(), Channel, Message]),
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
init([ReqId, From, Channel, Message]) ->
    State = #state{preflist=undefined,
                   req_id=ReqId,
                   coordinator=node(),
                   from=From,
                   channel=Channel,
                   message=Message,
                   num_responses=0,
                   pid_mappings=[]},
    {ok, prepare, State, 0}.

%% @doc Prepare request by retrieving the preflist.
prepare(timeout, #state{channel=Channel}=State) ->
    DocIdx = riak_core_util:chash_key({<<"subscriptions">>, Channel}),
    Preflist = riak_core_apl:get_apl(DocIdx, ?N, riak_pubsub_publish),
    {next_state, execute, State#state{preflist=Preflist}, 0}.

%% @doc Execute the request.
execute(timeout, #state{preflist=Preflist,
                        req_id=ReqId,
                        coordinator=Coordinator,
                        channel=Channel,
                        message=Message}=State) ->
    riak_pubsub_publish_vnode:publish(Preflist,
                                      {ReqId, Coordinator},
                                      Channel, Message),
    {next_state, waiting, State}.

%% @doc Pull a unique list of subscriptions from replicas, and
%%      relay the message to it.
waiting({ok, ReqId, PidMappings},
        #state{num_responses=NumResponses0,
               pid_mappings=PidMappings0,
               message=Message,
               from=From}=State0) ->
    lager:warning("Received pidmappings: ~p\n", [PidMappings]),
    NumResponses = NumResponses0 + 1,
    PidMappings1 = PidMappings0 ++ PidMappings,
    State = State0#state{num_responses=NumResponses,
                         pid_mappings=PidMappings1},
    case NumResponses =:= ?N of
        true ->
            Pids0 = lists:usort(fun({_, ChildPid1}, {_, ChildPid2}) ->
                            ChildPid1 =:= ChildPid2
                    end, PidMappings1),
            Pids = lists:map(fun({_, ChildPid}) -> ChildPid end,
                             Pids0),
            lager:warning("List of pids is: ~p", [Pids]),
            [Pid ! Message || Pid <- Pids],
            From ! {ReqId, ok},
            {stop, normal, State};
        false ->
            {next_state, waiting, State}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

mk_reqid() ->
    erlang:phash2(erlang:now()).
