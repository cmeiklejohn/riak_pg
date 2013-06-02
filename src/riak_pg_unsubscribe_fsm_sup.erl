%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Supervisor for the unsubscription requests.

-module(riak_pg_unsubscribe_fsm_sup).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/1,
         terminate_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start a child.
start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

%% @doc Stop a child immediately
terminate_child(Supervisor, Pid) ->
    supervisor:terminate_child(Supervisor, Pid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @doc supervisor callback.
init([]) ->
    UnsubscribeFSM = {riak_pg_unsubscribe_fsm,
                      {riak_pg_unsubscribe_fsm, start_link, []},
                       temporary, 5000, worker, [riak_pg_unsubscribe_fsm]},

    {ok, {{simple_one_for_one, 10, 10}, [UnsubscribeFSM]}}.
