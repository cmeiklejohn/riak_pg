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

-export([
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
  {ok, #state{node=node(), partition=Partition, groups=riak_dt_map:new()}}.


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
               #state{groups=Groups0, partition=Partition}=State) ->

  OldPids = proplists:get_value({Group, riak_dt_orswot}, riak_dt_map:value(Groups0), []),
  CurrentPids = riak_pg_util:prune(Pids ++ OldPids),
  OldGroups = case riak_dt_map:update(
                    {update,[{remove,{Group, riak_dt_orswot}}]},
                    Partition,
                    Groups0
                   ) of
               {ok, Groups1} ->
                 Groups1;
               _ ->
                 Groups0
             end,
  {ok, NewGroups} = riak_dt_map:update(
                   {update,[{update,{Group, riak_dt_orswot},{add_all, CurrentPids}}]},
                   Partition,
                   OldGroups
                  ),
  NewPruned = prune_empty_groups(NewGroups, Partition),
  {noreply, State#state{groups=NewPruned}};

%% @doc Respond to a members request.
handle_command({members, {ReqId, _}, Group},
               _Sender,
               #state{groups=Groups, partition=Partition, node=Node}=State) ->
  Pids = proplists:get_value({Group, riak_dt_orswot}, riak_dt_map:value(Groups), []),
  {reply, {ok, ReqId, {Partition, Node}, Pids}, State};

%% @doc Respond to a delete request.
handle_command({delete, {ReqId, _}, Group},
               _Sender,
               #state{groups=Groups0, partition=Partition}=State) ->
  Groups = case riak_dt_map:update(
                  {update, [{remove, {Group, riak_dt_orswot}}]},
                  Partition,
                  Groups0
                 ) of
             {ok, Groups1} ->
               Groups1;
             _ ->
               Groups0
           end,
  {reply, {ok, ReqId}, State#state{groups=Groups}};

%% @doc Respond to a join request.
handle_command({join, {ReqId, _}, Group, Pid},
               _Sender,
               #state{groups=Groups0, partition=Partition}=State) ->
  {ok, Groups} = riak_dt_map:update(
                   {update, [{update, {Group, riak_dt_orswot}, {add, Pid}}]},
                   Partition,
                   Groups0
                  ),
  {reply, {ok, ReqId}, State#state{groups=Groups}};

%% @doc Respond to a leave request.
handle_command({leave, {ReqId, _}, Group, Pid},
               _Sender,
               #state{groups=Groups0, partition=Partition}=State) ->
  NewGroups = case riak_dt_map:update(
                     {update, [{update, {Group, riak_dt_orswot}, {remove, Pid}}]},
                     Partition,
                     Groups0
                    ) of
                {ok, Groups} ->
                  Groups;
                _ ->
                  Groups0
              end,
  NewPruned = prune_empty_groups(NewGroups, Partition),
  {reply, {ok, ReqId}, State#state{groups=NewPruned}};


%% @doc Default handler.
handle_command(Message, _Sender, State) ->
  ?PRINT({unhandled_command, Message}),
  {noreply, State}.

%% @doc Fold over the dict for handoff.
handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
  Acc = lists:foldl(Fun, Acc0, riak_dt_map:value(State#state.groups)),
  {reply, Acc, State}.

handoff_starting(_TargetNode, State) ->
  {true, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(_TargetNode, State) ->
  {ok, State}.

%% @doc Handle receiving data from handoff.
handle_handoff_data(Data,
                    #state{groups=Groups0,partition=Partition}=State) ->
  {{Group,riak_dt_orswot}, Pids} = binary_to_term(Data),
  {ok, Groups} = riak_dt_map:update(
                   {update, [{update,{Group, riak_dt_orswot}, {add_all, Pids}}]},
                   Partition,
                   Groups0
                  ),
  {reply, ok, State#state{groups=Groups}}.

encode_handoff_item(Group, Pids) ->
  term_to_binary({Group, Pids}).

is_empty(#state{groups=Groups}=State) ->
  case length(riak_dt_map:value(Groups)) of
    0 ->
      {true, State};
    _ ->
      {false, State}
  end.

delete(State) ->
  {ok, State}.

handle_coverage(groups, _KeySpaces, _Sender, State=#state{groups=Groups,partition=Partition}) ->
  NewGroups = prune_empty_groups(Groups,Partition),
  GroupList = [Group || {{Group, riak_dt_orswot}, _Pids}
                          <- riak_dt_map:value(NewGroups)],
  {reply, GroupList, State#state{groups=NewGroups}};

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
  {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

prune_empty_groups(CRDT, Partition) ->
  prune_empty_groups(CRDT, Partition, riak_dt_map:value(CRDT)).

prune_empty_groups(CRDT, Partition, [{{_Group, riak_dt_orswot},Pids}|List])
  when length(Pids) =/= 0->
  prune_empty_groups(CRDT, Partition, List);
prune_empty_groups(CRDT0, Partition, [{{Group, riak_dt_orswot},_Pids}|List]) ->
  {ok, CRDT} = riak_dt_map:update(
           {update, [{remove, {Group, riak_dt_orswot}}]},
           Partition,
           CRDT0
          ),
  prune_empty_groups(CRDT, Partition, List);
prune_empty_groups(CRDT, _Partition, []) ->
  CRDT.
