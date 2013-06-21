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
%% @doc Gproc helper functions.

-module(riak_pg_gproc).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-include_lib("riak_pg.hrl").

-export([key/2,
         store/2]).

%% @doc Generate a gproc registration key.
-spec key(term(), riak_core:partition()) -> gproc:key().
key(Group, Partition) ->
    {p, l, {riak_pg_membership, Group, Partition}}.

%% @doc Store a value in gproc.
-spec store(gproc:key(), term()) -> ok.
store(Key, Value) ->
    try
        true = gproc:set_value(Key, Value),
        ok
    catch
        _:_ ->
            true = gproc:reg(Key, Value),
            ok
    end.
