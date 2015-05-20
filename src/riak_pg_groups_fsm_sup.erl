-module('riak_pg_groups_fsm_sup').

-behaviour(supervisor).

-export([start_child/1,terminate_child/2]).
-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
  supervisor:start_child(?MODULE, Args).

terminate_child(Supervisor, Pid) ->
  supervisor:terminate_child(Supervisor, Pid).

init([]) ->
    GroupsFsmSpec = {undefined,
                   {riak_core_coverage_fsm, start_link, [riak_pg_groups_fsm]},
                   temporary, 5000, worker, [riak_pg_groups_fsm]},

  {ok, {{simple_one_for_one, 10, 10}, [GroupsFsmSpec]}}.
