%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Application supervisor.

-module(riak_pubsub_sup).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = {riak_pubsub_vnode_master,
               {riak_core_vnode_master, start_link, [riak_pubsub_vnode]},
                permanent, 5000, worker, [riak_core_vnode_master]},

    Publish = {riak_pubsub_publish_vnode_master,
               {riak_core_vnode_master, start_link, [riak_pubsub_publish_vnode]},
                permanent, 5000, worker, [riak_core_vnode_master]},

    Subscribe = {riak_pubsub_subscribe_vnode_master,
                 {riak_core_vnode_master, start_link, [riak_pubsub_subscribe_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    PublishFSM = {riak_pubsub_publish_fsm_sup,
                  {riak_pubsub_publish_fsm_sup, start_link, []},
                   permanent, infinity, supervisor, [riak_pubsub_publish_fsm_sup]},

    SubscribeFSM = {riak_pubsub_subscribe_fsm_sup,
                    {riak_pubsub_subscribe_fsm_sup, start_link, []},
                     permanent, infinity, supervisor, [riak_pubsub_subscribe_fsm_sup]},

    {ok, {{one_for_one, 5, 10}, [VMaster,
                                 Publish,
                                 Subscribe,
                                 PublishFSM,
                                 SubscribeFSM]}}.
