%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Gproc helper functions.

-module(riak_pubsub_gproc).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-include_lib("riak_pubsub.hrl").

-export([key/2,
         store/2]).

%% @doc Generate a gproc registration key.
-spec key(term(), riak_core:partition()) -> gproc:key().
key(Channel, Partition) ->
    {p, l, {riak_pubsub_subscription, Channel, Partition}}.

%% @doc Store a value in gproc.
-spec store(gproc:key(), term()) -> ok.
store(Key, Value) ->
    lager:warning("Storing in gproc: ~p ~p.\n", [Key, Value]),

    try
        true = gproc:set_value(Key, Value),
        ok
    catch
        _:_ ->
            true = gproc:reg(Key, Value),
            ok
    end.
