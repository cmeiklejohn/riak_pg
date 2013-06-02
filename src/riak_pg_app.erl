%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Application.

-module(riak_pg_app).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case riak_pg_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register(riak_pg,
                                    [{vnode_module, riak_pg_vnode}]),
            ok = riak_core_node_watcher:service_up(riak_pg, self()),

            ok = riak_core:register(riak_pg_messaging,
                                    [{vnode_module, riak_pg_messaging_vnode}]),
            ok = riak_core_node_watcher:service_up(riak_pg_messaging, self()),

            ok = riak_core:register(riak_pg_memberships,
                                    [{vnode_module, riak_pg_memberships_vnode}]),
            ok = riak_core_node_watcher:service_up(riak_pg_memberships, self()),

            ok = riak_core:register(riak_pg_message_proxy,
                                    [{vnode_module, riak_pg_message_proxy_vnode}]),
            ok = riak_core_node_watcher:service_up(riak_pg_message_proxy, self()),

            ok = riak_core_ring_events:add_guarded_handler(
                    riak_pg_ring_event_handler, []),

            ok = riak_core_node_watcher_events:add_guarded_handler(
                    riak_pg_node_event_handler, []),

            EntryRoute = {["riak_pg"], riak_pg_wm_ping, []},
            webmachine_router:add_route(EntryRoute),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
