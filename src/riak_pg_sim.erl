%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Simulations; taken from riak_dt.

-module(riak_pg_sim).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-export([test/2,
         part/0,
         heal/0]).

%% @doc Test a round trip.
test(Group, Message) ->
    Pid = self(),

    case riak_pg:join(Group, Pid) of
        {error, timeout} ->
            false;
        _ ->
            {ok, _} = riak_pg:send(Group, Message),
            ok = flush(),
            ok = riak_pg:leave(Group, Pid),
            true
    end.

%% @doc Partition.
part() ->
    true = rpc:call('riak_pg@127.0.0.1',
                    erlang,
                    set_cookie,
                    ['riak_pg2@127.0.0.1', riak_pg2]),
    true = erlang:set_cookie(node(),
                             riak_pg2),
    true = rpc:call('riak_pg2@127.0.0.1',
                    erlang,
                    disconnect_node,
                    ['riak_pg3@127.0.0.1']),
    true = rpc:call('riak_pg2@127.0.0.1',
                    erlang,
                    disconnect_node,
                    ['riak_pg4@127.0.0.1']),
    true = erlang:disconnect_node('riak_pg3@127.0.0.1'),
    true = erlang:disconnect_node('riak_pg4@127.0.0.1').

%% @doc Heal.
heal() ->
    true = rpc:call('riak_pg2@127.0.0.1',
                    erlang,
                    set_cookie,
                    ['riak_pg2@127.0.0.1', riak_pg]),
    true = erlang:set_cookie(node(),
                             riak_pg).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Flush the message queue immediately.
-spec flush() -> ok.
flush() ->
    receive
        Result ->
            lager:warning("Received: ~p", [Result]),
            flush()
    after
        0 ->
            ok
    end.

