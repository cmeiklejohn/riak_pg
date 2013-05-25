%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Application.

-module(riak_pubsub).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-include_lib("riak_pubsub.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional.
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>,
                                       term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, riak_pubsub),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode,
                                              ping,
                                              riak_pubsub_vnode_master).
