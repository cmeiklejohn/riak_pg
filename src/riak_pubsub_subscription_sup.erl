%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Supervisor for the subscriptions.

-module(riak_pubsub_subscription_sup).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/2,
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
start_child(Channel, Pid) ->
    lager:warning("Starting child to send to ~p on channel ~p.\n",
                  [Pid, Channel]),
    supervisor:start_child(?MODULE, [Channel, Pid]).

%% @doc Stop a child immediately
terminate_child(Supervisor, Pid) ->
    supervisor:terminate_child(Supervisor, Pid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @doc supervisor callback.
init([]) ->
    SubscriptionSup = {riak_pubsub_subscription,
                        {riak_pubsub_subscription, start_link, []},
                         temporary, 5000, worker, [riak_pubsub_subscription]},

    {ok, {{simple_one_for_one, 10, 10}, [SubscriptionSup]}}.
