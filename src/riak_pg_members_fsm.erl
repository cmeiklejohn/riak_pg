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
%% @copyright 2013 Christopher Meiklejohn.
%% @copryright 2015 Mark Steele.
%% @doc Members FSM.

-module(riak_pg_members_fsm).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').
-author('Mark Steele <mark@control-alt-del.org>').

-behaviour(gen_fsm).

-include("riak_pg.hrl").

%% API
-export([start_link/3,
         members/1]).

%% Callbacks
-export([init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3]).

%% States
-export([prepare/2,
         execute/2,
         waiting/2,
         waiting_n/2,
         finalize/2]).

-record(state, {preflist,
                req_id,
                coordinator,
                from,
                group,
                groups,
                num_responses,
                replies,
                prune}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReqId, From, Group) ->
  gen_fsm:start_link(?MODULE, [ReqId, From, Group], []).

%% @doc Get members.
members(Group) ->
  ReqId = riak_pg:mk_reqid(),
  _ = riak_pg_members_fsm_sup:start_child([ReqId, self(), Group]),
  {ok, ReqId}.

%%%===================================================================
%%% Callbacks
%%%===================================================================

handle_info(_Info, _StateName, StateData) ->
  {stop, badmsg, StateData}.

handle_event(_Event, _StateName, StateData) ->
  {stop, badmsg, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
  {stop, badmsg, StateData}.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
  ok.

%%%===================================================================
%%% States
%%%===================================================================

%% @doc Initialize the request.
init([ReqId, From, Group]) ->
  State = #state{preflist=undefined,
                 req_id=ReqId,
                 coordinator=node(),
                 from=From,
                 group=Group,
                 num_responses=0,
                 replies=[]},
  {ok, prepare, State, 0}.

%% @doc Prepare request by retrieving the preflist.
prepare(timeout, #state{group=Group}=State) ->
  DocIdx = riak_core_util:chash_key({<<"pg">>, Group}),
  Preflist = riak_core_apl:get_primary_apl(DocIdx, ?N, riak_pg),
  Preflist2 = [{Index, Node} || {{Index, Node}, _Type} <- Preflist],
  {next_state, execute, State#state{preflist=Preflist2}, 0}.

%% @doc Execute the request.
execute(timeout, #state{preflist=Preflist,
                        req_id=ReqId,
                        coordinator=Coordinator}=State) ->
  riak_pg_vnode:members(Preflist, {ReqId, Coordinator}),
  {next_state, waiting, State}.

%% @doc Pull a unique list of memberships from replicas, and
%%      relay the message to it.
waiting({ok, _ReqId, IndexNode, Reply},
        #state{from=From,
               req_id=ReqId,
               group = Group,
               num_responses=NumResponses0,
               replies=Replies0}=State0) ->
  NumResponses = NumResponses0 + 1,
  Replies = [{IndexNode, Reply}|Replies0],
  State = State0#state{num_responses=NumResponses, replies=Replies},

  case NumResponses =:= ?R of
    true ->
      {Prune, Pids} = merge_pids(Replies, Group),
      From ! {ReqId, ok, Pids},
      {next_state, waiting_n, State#state{prune = Prune}};
    false ->
      {next_state, waiting, State}
  end.

%% @doc Wait for the remainder of responses from replicas.
waiting_n({ok, _ReqId, IndexNode, Reply},
          #state{num_responses=NumResponses0,
                 replies=Replies0}=State0) ->
  NumResponses = NumResponses0 + 1,
  Replies = [{IndexNode, Reply}|Replies0],
  State = State0#state{num_responses=NumResponses, replies=Replies},

  case NumResponses =:= ?N of
    true ->
      {next_state, finalize, State, 0};
    false ->
      {next_state, waiting_n, State}
  end.

%% @doc Perform read repair.
finalize(timeout, #state{replies=Replies}=State) ->
  Merged = merge(Replies),
  ok = repair(Replies, State#state{groups = Merged}),
  case State#state.prune of
    true ->
      [riak_pg_vnode:prune(IndexNode) || {IndexNode, _Map} <- Replies];
    false ->
      ok
  end,
  {stop, normal, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc perform merge of replica CRDT maps
merge([{_,H}|T]) ->
  merge(T,H).
merge([], Merged) ->
  Merged;
merge([{_,H}|T], Merged) ->
  merge(T, riak_dt_map:merge(H, Merged)).

%% @doc Perform merge of replica pids.
merge_pids(Replies, Group) ->
  Pids = lists:sort(
           proplists:get_value(
             {Group, riak_dt_orswot},
             riak_dt_map:value(merge(Replies)),
             []
            )
          ),
  Pruned = lists:sort(riak_pg_util:prune(Pids)),
  {Pids == Pruned, Pruned}.

%% @doc Trigger repair if necessary.
repair([{IndexNode, Map}|Replies], State) ->
  case riak_dt_map:equal(Map, State#state.groups) of
    true ->
      ok;
    false ->
      riak_pg_vnode:repair(IndexNode, State#state.groups)
  end,
  repair(Replies, State);

repair([], _State) ->
  ok.
