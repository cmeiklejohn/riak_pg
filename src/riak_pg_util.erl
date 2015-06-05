-module(riak_pg_util).

-export([prune/1]).

%% @doc If the node is connected, and the process is not alive, prune
%%      it.
prune_pid(Pid) when is_pid(Pid) ->
  lists:member(node(Pid), nodes())
    andalso (is_process_alive(node(Pid), Pid) =:= false).

%% @doc Remote call to determine if process is alive or not; assume if
%%      the node fails communication it is, since we have no proof it
%%      is not.
is_process_alive(Node, Pid) ->
  case rpc:call(Node, erlang, is_process_alive, [Pid]) of
    {badrpc, _} ->
      true;
    Value ->
      Value
  end.

%% @doc Based on connected nodes, prune out processes that no longer
%%      exist.
prune(List) ->
  lists:foldl(
    fun(Pid, Acc) ->
        case prune_pid(Pid) of
          true ->
            [Pid|Acc];
          false ->
            Acc
        end
    end,
    [],
    List).
