%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Publish FSM.

-module(riak_pubsub_publish_fsm).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(gen_fsm).

-include_lib("riak_pubsub.hrl").

%% API
-export([start_link/4,
         publish/2]).

%% Callbacks
-export([init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3]).

%% States
-export([prepare/2,
         execute/2]).

-record(state, {preflist,
                req_id,
                coordinator,
                from,
                channel,
                message,
                responses}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReqId, From, Channel, Message) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Channel, Message], []).

publish(Channel, Message) ->
    ReqId = mk_reqid(),
    riak_pubsub_publish_fsm_sup:start_child(
        [ReqId, self(), Channel, Message]),
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
init([ReqId, From, Channel, Message]) ->
    State = #state{preflist=undefined,
                   req_id=ReqId,
                   coordinator=node(),
                   from=From,
                   channel=Channel,
                   message=Message,
                   responses=0},
    {ok, prepare, State, 0}.

%% @doc Prepare request by retrieving the preflist.
prepare(timeout, #state{channel=Channel}=State) ->
    DocIdx = riak_core_util:chash_key({<<"subscriptions">>, Channel}),
    Preflist = riak_core_apl:get_apl(DocIdx, ?N, riak_pubsub_publish),
    {next_state, execute, State#state{preflist=Preflist}, 0}.

%% @doc Execute the request.
execute(timeout, #state{preflist=Preflist0,
                        req_id=ReqId,
                        coordinator=Coordinator,
                        channel=Channel,
                        from=From,
                        message=Message}=State) ->
    [IndexNode|Preflist] = Preflist0,

    lager:warning("Publishing to ~p.\n",
                  [IndexNode]),

    case riak_pubsub_publish_vnode:publish(IndexNode,
                                           {ReqId, Coordinator},
                                           Channel, Message) of
        {error, timeout} ->
            lager:warning("Publishing to ~p failed with timeout.\n",
                          [IndexNode]),

            case Preflist of
                [] ->
                    lager:warning("Failed: preflist_exhausted.\n"),

                    From ! {ReqId, ok, {error, preflist_exhausted}},
                    {stop, normal, State};
                _ ->
                    lager:warning("Moving to next index.\n"),

                    {next_state, execute,
                     State#state{preflist=Preflist},
                     0}
            end;
        _ ->
            lager:warning("Successful publication.\n"),

            From ! {ReqId, ok},
            {stop, normal, State}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

mk_reqid() ->
    erlang:phash2(erlang:now()).
