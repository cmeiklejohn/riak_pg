%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Christopher Meiklejohn, 2015 Mark Steele  All Rights Reserved.
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
%% @author Mark Steele <mark@control-alt-del.org>
%% @copyright 2013 Christopher Meiklejohn
%% @copyright 2015 Mark Steele
%% @doc pg vnode.

-module(riak_pg_vnode).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').
-author('Mark Steele <mark@control-alt-del.org>').

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
         prune/1,
         repair/2,
         members/2]).


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
members(Preflist, Identity) ->
  riak_core_vnode_master:command(Preflist,
                                 {members, Identity},
                                 {fsm, undefined, self()},
                                 ?MASTER).

%% @doc Perform repair.
repair(IndexNode, Map) ->
  riak_core_vnode_master:command(IndexNode,
                                 {repair, Map},
                                 ignore,
                                 ?MASTER).

%% @doc Trigger pruning
prune(IndexNode) ->
  riak_core_vnode_master:command(IndexNode,
                                 prune,
                                 ignore,
                                 ?MASTER).

groups(Preflist, ReqId) ->
  riak_core_vnode_master:coverage({groups, ReqId},
                                  Preflist,
                                  all,
                                  {fsm, undefined, self()},
                                  ?MASTER).

%% @doc Perform join as part of repair.
handle_command({repair, Map},
               _Sender,
               #state{groups=Groups0, partition=Partition}=State) ->
  NewGroups = riak_dt_map:merge(Groups0, Map),
  NewPruned = prune_groups(NewGroups, Partition),
  {noreply, State#state{groups=NewPruned}};

handle_command(prune,
               _Sender,
               #state{groups=Groups0, partition=Partition}=State) ->
  Pruned = prune_groups(Groups0, Partition),
  {noreply, State#state{groups=Pruned}};

%% @doc Respond to a members request.
handle_command({members, {ReqId, _}},
               _Sender,
               #state{groups=Groups, partition=Partition, node=Node}=State) ->
  {reply, {ok, ReqId, {Partition, Node}, Groups}, State};

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
  {reply, {ok, ReqId}, State#state{groups=NewGroups}};


%% @doc Default handler.
handle_command(Message, _Sender, State) ->
  ?PRINT({unhandled_command, Message}),
  {noreply, State}.

%% @doc Fold over the dict for handoff.
handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
  Acc = lists:foldl(Fun, Acc0, [{foo,State#state.groups}]),
  {reply, Acc, State}.

handoff_starting(_TargetNode, State) ->
  {true, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(_TargetNode, State) ->
  {ok, State}.

%% @doc Handle receiving data from handoff.
handle_handoff_data(Data, #state{groups=Groups0}=State) ->
  CRDT = riak_dt:from_binary(Data),
  Groups = riak_dt_map:merge(Groups0, CRDT),
  {reply, ok, State#state{groups=Groups}}.

encode_handoff_item(_, Data) ->
  riak_dt:to_binary(Data).

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
  NewGroups = prune_groups(Groups,Partition),
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

prune_groups(CRDT, Partition) ->
  prune_groups(CRDT, Partition, riak_dt_map:value(CRDT)).

prune_groups(CRDT0, Partition, [{{Group, riak_dt_orswot},Pids}|List])
  when length(Pids) =/= 0->
  Pruned = riak_pg_util:prune(Pids),
  case length(Pruned) =:= length(Pids) of
    true ->
      prune_groups(CRDT0, Partition, List);
    false ->
      case length(Pruned) =/= 0 of
        true ->
          Remove = lists:filter(
                     fun(X) ->
                         not lists:member(X, Pruned)
                     end,
                     Pids),
          {ok, CRDT} = riak_dt_map:update(
                         {update, [{update,
                                    {Group, riak_dt_orswot},
                                    {remove,Remove}
                                   }]},
                         Partition,
                         CRDT0
                        );
        false ->
          {ok, CRDT} = riak_dt_map:update(
                         {update, [{remove, {Group, riak_dt_orswot}}]},
                         Partition,
                         CRDT0
                        )
      end,
      prune_groups(CRDT, Partition, List)
  end;

prune_groups(CRDT0, Partition, [{{Group, riak_dt_orswot},_Pids}|List]) ->
  {ok, CRDT} = riak_dt_map:update(
           {update, [{remove, {Group, riak_dt_orswot}}]},
           Partition,
           CRDT0
          ),
  prune_groups(CRDT, Partition, List);
prune_groups(CRDT, _Partition, []) ->
  CRDT.
