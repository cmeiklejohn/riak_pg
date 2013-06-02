%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Application.

-module(riak_pg).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-include_lib("riak_pg.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-define(TIMEOUT, 5000).

-export([ping/0,
         publish/2,
         subscribe/2,
         unsubscribe/2]).

-export([mk_reqid/0,
         wait_for_reqid/2]).

%% Public API

%% @doc Publish updates on a given channel.
publish(Channel, Message) ->
    {ok, ReqId} = riak_pg_publish_fsm:publish(Channel, Message),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Subscribe to updates on a given channel.
subscribe(Channel, Pid) ->
    {ok, ReqId} = riak_pg_subscribe_fsm:subscribe(Channel, Pid),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Subscribe to updates on a given channel.
unsubscribe(Channel, Pid) ->
    {ok, ReqId} = riak_pg_unsubscribe_fsm:unsubscribe(Channel, Pid),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Pings a random vnode to make sure communication is functional.
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>,
                                       term_to_binary(now())}),
    Preflist = riak_core_apl:get_primary_apl(DocIdx, 1, riak_pg),
    [{IndexNode, _Type}] = Preflist,
    riak_core_vnode_master:sync_spawn_command(IndexNode,
                                              ping,
                                              riak_pg_vnode_master).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Generate a request id.
mk_reqid() ->
    erlang:phash2(erlang:now()).

%% @doc Wait for a response.
wait_for_reqid(ReqID, Timeout) ->
    receive
        {ReqID, ok} ->
            ok;
        {ReqID, ok, Val} ->
            {ok, Val}
    after Timeout ->
        {error, timeout}
    end.
