%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Christopher Meiklejohn.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
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
         ping/0,
         groups/0,
         members/1,
         local_members/1,
         connected_members/1]).

-export([mk_reqid/0,
         wait_for_reqid/2]).

%% Public API

%% @doc Create a group.
-spec create(term()) -> ok | {error, timeout}.
create(Group) ->
    {ok, ReqId} = riak_pg_create_fsm:create(Group),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Delete a group.
-spec delete(term()) -> ok | {error, timeout}.
delete(Group) ->
    {ok, ReqId} = riak_pg_delete_fsm:delete(Group),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Join pid to group.
-spec join(term(), pid()) -> ok | {error, timeout}.
join(Group, Pid) ->
    {ok, ReqId} = riak_pg_join_fsm:join(Group, Pid),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Remove pid from group.
-spec leave(term(), pid()) -> ok | {error, timeout}.
leave(Group, Pid) ->
    {ok, ReqId} = riak_pg_leave_fsm:leave(Group, Pid),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Return a listing of all registered groups.
%% @todo
-spec groups() -> ok.
groups() ->
    ok.

%% @doc Return a listing of members of a particular group.
-spec members(term()) -> {ok, list(pid())} | {error, timeout}.
members(Group) ->
    {ok, ReqId} = riak_pg_members_fsm:members(Group),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @doc Return a listing of local members of a particular group.
-spec local_members(term()) -> {ok, list(pid())} | {error, timeout}.
local_members(Group) ->
    {ok, ReqId} = riak_pg_members_fsm:members(Group),
    case wait_for_reqid(ReqId, ?TIMEOUT) of
        {ok, Members} ->
            LocalMembers = lists:filter(fun(Pid) ->
                            node(Pid) =:= node() end, Members),
            {ok, LocalMembers};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Return a listing of connected members of a particular group.
-spec connected_members(term()) -> {ok, list(pid())} | {error, timeout}.
connected_members(Group) ->
    {ok, ReqId} = riak_pg_members_fsm:members(Group),
    case wait_for_reqid(ReqId, ?TIMEOUT) of
        {ok, Members} ->
            ConnectedMembers = lists:filter(fun(Pid) ->
                            lists:member(node(Pid), nodes()) end, Members),
            {ok, ConnectedMembers};
        {error, Error} ->
            {error, Error}
    end.

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
