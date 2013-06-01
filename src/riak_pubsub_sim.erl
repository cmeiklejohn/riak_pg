%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Simulations; taken from riak_dt.

-module(riak_pubsub_sim).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-export([test/2,
         part/0,
         heal/0]).

%% @doc Test a round trip.
test(Channel, Message) ->
    Pid = self(),

    case riak_pubsub:subscribe(Channel, Pid) of
        {error, timeout} ->
            false;
        _ ->
            {ok, _} = riak_pubsub:publish(Channel, Message),
            ok = flush(),
            ok = riak_pubsub:unsubscribe(Channel, Pid),
            true
    end.

%% @doc Partition.
part() ->
    true = rpc:call('riak_pubub@127.0.0.1',
                    erlang,
                    set_cookie,
                    ['riak_pubsub2@127.0.0.1', riak_pubsub2]),
    true = erlang:set_cookie(node(),
                             riak_pubsub2),
    true = rpc:call('riak_pubsub2@127.0.0.1',
                    erlang,
                    disconnect_node,
                    ['riak_pubsub3@127.0.0.1']),
    true = rpc:call('riak_pubsub2@127.0.0.1',
                    erlang,
                    disconnect_node,
                    ['riak_pubsub4@127.0.0.1']),
    true = erlang:disconnect_node('riak_pubsub3@127.0.0.1'),
    true = erlang:disconnect_node('riak_pubsub4@127.0.0.1').

%% @doc Heal.
heal() ->
    true = rpc:call('riak_pubsub2@127.0.0.1',
                    erlang,
                    set_cookie,
                    ['riak_pubsub2@127.0.0.1', riak_pubsub]),
    true = erlang:set_cookie(node(),
                             riak_pubsub).

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

