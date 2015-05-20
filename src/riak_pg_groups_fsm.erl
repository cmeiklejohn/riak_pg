-module(riak_pg_groups_fsm).

-behaviour(riak_core_coverage_fsm).

-include("riak_pg.hrl").

-export([init/2,
         process_results/2,
         finish/2,
         groups/0]).

-record(state, {results = [],from}).
-define(TIMEOUT, 10000).

groups() ->
    ReqId = riak_pg:mk_reqid(),
    riak_pg_groups_fsm_sup:start_child([{raw, ReqId, self()},[?TIMEOUT]]),
    {ok, ReqId}.

init(From,[Timeout]) ->
  {
    groups,
    all,
    ?N,
    ?R,
    riak_pg,
    riak_pg_vnode_master,
    Timeout,
    #state{from=From}
  }.

process_results(done, State) ->
  {done, State};
process_results(Groups, StateData=#state{results=Results}) ->
  {done, StateData#state{results=lists:append(Groups, Results)}};
process_results({error, Reason}, _State) ->
  {error, Reason};
process_results(Message, State) ->
  lager:info("Unhandled result: ~p", [Message]),
  {ok, State}.

finish({error, Error}, StateData=#state{from={raw, ReqId, ClientPid}}) ->
  lager:info("Error in coverage request: ~p~n", [Error]),
  ClientPid ! {ReqId, Error},
  {stop, normal, StateData};

finish(clean, StateData=#state{results=Results,from={raw, ReqId, ClientPid}}) ->
  List = lists:usort(Results),
  ClientPid ! {ReqId, ok, List},
  {stop, normal, StateData};

finish(Message, StateData) ->
  lager:info("Unhandled finish: ~p~n", [Message]),
  {stop, normal, StateData}.
