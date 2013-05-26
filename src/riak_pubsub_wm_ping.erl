%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Ping resource.

-module(riak_pubsub_wm_ping).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-export([init/1,
         to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, nostate}.

to_html(ReqData, Context) ->
    Result = io_lib:format("Result: ~p", [riak_pubsub:ping()]),
    {"<html><head><title>{{appid}}</title></head><body>" ++ Result ++ "</body></html>", ReqData, Context}.
