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
%% @doc Memberships vnode.

-module(riak_pg_vnode).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(riak_core_vnode).

-include("riak_pg.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-export([create/3,
         delete/3,
         join/4,
         leave/4,
         groups/2,
         members/3]).

-export([repair/3]).

-record(state, {node, partition, groups}).

-define(MASTER,riak_pg_vnode_master).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state{node=node(), partition=Partition, groups=dict:new()}}.

%% @doc Create group.
create(Preflist, Identity, Group) ->
    riak_core_vnode_master:command(Preflist,
                                   {create, Identity, Group},
                                   {fsm, undefined, self()},
                                   ?MASTER).

%% @doc Delete group.
delete(Preflist, Identity, Group) ->
    riak_core_vnode_master:command(Preflist,
                                   {delete, Identity, Group},
                                   {fsm, undefined, self()},
                                   ?MASTER).

%% @doc Join group.
join(Preflist, Identity, Group, Pid) ->
    riak_core_vnode_master:command(Preflist,
                                   {join, Identity, Group, Pid},
                                   {fsm, undefined, self()},
                                   ?MASTER).

%% @doc Leave group.
leave(Preflist, Identity, Group, Pid) ->
    riak_core_vnode_master:command(Preflist,
                                   {leave, Identity, Group, Pid},
                                   {fsm, undefined, self()},
                                   ?MASTER).

%% @doc Group members.
members(Preflist, Identity, Group) ->
    riak_core_vnode_master:command(Preflist,
                                   {members, Identity, Group},
                                   {fsm, undefined, self()},
                                   ?MASTER).

%% @doc Perform repair.
repair(IndexNode, Group, Pids) ->
    riak_core_vnode_master:command(IndexNode,
                                   {repair, Group, Pids},
                                   ignore,
                                   ?MASTER).

groups(Preflist, ReqId) ->
  riak_core_vnode_master:coverage({groups, ReqId},
                                  Preflist,
                                  all,
                                  {fsm, undefined, self()},
                                  ?MASTER).

%% @doc Perform join as part of repair.
handle_command({repair, Group, Pids},
               _Sender,
               #state{groups=Groups0}=State) ->
    %% Store back into the dict.
    Groups = dict:store(Group, Pids, Groups0),

    {noreply, State#state{groups=Groups}};

%% @doc Respond to a members request.
handle_command({members, {ReqId, _}, Group},
               _Sender,
               #state{groups=Groups, partition=Partition, node=Node}=State) ->
    %% Find existing list of Pids.
    Pids = pids(Groups, Group, riak_dt_orswot:new()),

    %% Return updated groups.
    {reply, {ok, ReqId, {Partition, Node}, Pids}, State};

%% @doc Respond to a delete request.
handle_command({delete, {ReqId, _}, Group},
               _Sender,
               #state{groups=Groups0}=State) ->
    %% Find existing list of Pids, and add object to it.
    Pids = riak_dt_orswot:new(),

    %% Store back into the dict.
    Groups = dict:store(Group, Pids, Groups0),

    %% Return updated groups.
    {reply, {ok, ReqId}, State#state{groups=Groups}};

%% @doc Respond to a create request.
handle_command({create, {ReqId, _}, Group},
               _Sender,
               #state{groups=Groups0}=State) ->
    %% Find existing list of Pids, and add object to it.
    Pids = pids(Groups0, Group, riak_dt_orswot:new()),

    %% Store back into the dict.
    Groups = dict:store(Group, Pids, Groups0),

    %% Return updated groups.
    {reply, {ok, ReqId}, State#state{groups=Groups}};

%% @doc Respond to a join request.
handle_command({join, {ReqId, _}, Group, Pid},
               _Sender,
               #state{groups=Groups0, partition=Partition}=State) ->
    %% Find existing list of Pids, and add object to it.
    Pids0 = pids(Groups0, Group, riak_dt_orswot:new()),
    {ok, Pids} = riak_dt_orswot:update({add, Pid}, Partition, Pids0),

    %% Store back into the dict.
    Groups = dict:store(Group, Pids, Groups0),

    %% Return updated groups.
    {reply, {ok, ReqId}, State#state{groups=Groups}};

%% @doc Respond to a leave request.
handle_command({leave, {ReqId, _}, Group, Pid},
               _Sender,
               #state{groups=Groups0, partition=Partition}=State) ->
    %% Find existing list of Pids, and add object to it.
    Pids0 = pids(Groups0, Group, riak_dt_orswot:new()),
    {ok, Pids} = riak_dt_orswot:update({remove, Pid}, Partition, Pids0),

    %% Store back into the dict.
    Groups = dict:store(Group, Pids, Groups0),

    {reply, {ok, ReqId}, State#state{groups=Groups}};

%% @doc Default handler.
handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

%% @doc Fold over the dict for handoff.
handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Acc = dict:fold(Fun, Acc0, State#state.groups),
    {reply, Acc, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

%% @doc Handle receiving data from handoff.  Decode data and
%%      perform join/leave.
handle_handoff_data(Data,
                    #state{groups=Groups0}=State) ->
    {Group, Pids} = binary_to_term(Data),

    %% Find existing list of Pids, and add object to it.
    Pids0 = pids(Groups0, Group, riak_dt_orswot:new()),
    MPids = riak_dt_orswot:merge(Pids, Pids0),

    %% Store back into the dict.
    Groups = dict:store(Group, MPids, Groups0),

    {reply, ok, State#state{groups=Groups}}.

encode_handoff_item(Group, Pids) ->
    term_to_binary({Group, Pids}).

is_empty(#state{groups=Groups}=State) ->
    case dict:size(Groups) of
        0 ->
            {true, State};
        _ ->
            {false, State}
    end.

delete(State) ->
    {ok, State}.

handle_coverage(groups, _KeySpaces, _Sender, State=#state{groups=Groups}) ->
  {reply, dict:fetch_keys(Groups), State};
handle_coverage(_Req, _KeySpaces, _Sender, State) ->
  {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Return pids from the dict.
-spec pids(dict:dict(), atom(), term()) -> term().
pids(Groups, Group, Default) ->
    case dict:find(Group, Groups) of
        {ok, Object} ->
            Object;
        _ ->
            Default
    end.
