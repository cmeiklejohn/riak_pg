%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Application.

-module(riak_pg).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-include_lib("riak_pg.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-define(TIMEOUT, 5000).

-export([create/1,
         delete/1,
         join/2,
         leave/2,
         send/2,
         ping/0,
         groups/0,
         members/1,
         local_members/1]).

-export([mk_reqid/0,
         wait_for_reqid/2]).

%% Public API

%% @doc Create a group.
create(Group) ->
    {ok, ReqId} = riak_pg_create_fsm:send(Group),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Delete a group.
delete(Group) ->
    {ok, ReqId} = riak_pg_delete_fsm:send(Group),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Send a message to the group.
send(Group, Message) ->
    {ok, ReqId} = riak_pg_send_fsm:send(Group, Message),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Join pid to group.
join(Group, Pid) ->
    {ok, ReqId} = riak_pg_join_fsm:join(Group, Pid),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Remove pid from group.
leave(Group, Pid) ->
    {ok, ReqId} = riak_pg_leave_fsm:leave(Group, Pid),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Return a listing of all registered groups.
%% @todo
groups() ->
    ok.

%% @doc Return a listing of members of a particular group.
members(Group) ->
    {ok, ReqId} = riak_pg_members_fsm:members(Group),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Return a listing of lcoal members of a particular group.
local_members(Group) ->
    {ok, ReqId} = riak_pg_local_members_fsm:local_members(Group),
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
