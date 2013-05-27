%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2013 Christopher Meiklejohn.
%% @doc Subscription.

-module(riak_pubsub_subscription).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {partition, channel, pid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Partition, Channel, Pid) ->
    Name = atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Channel) ++ "_"
           ++ pid_to_list(Pid) ++ "_" ++ integer_to_list(Partition),
    gen_server:start_link({local, list_to_atom(Name)},
                          ?MODULE, [Partition, Channel, Pid], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Partition, Channel, Pid]) ->
    lager:warning("Registering ~p to send to ~p on channel ~p.\n",
                  [self(), Pid, Channel]),

    %% Tag this process with a local property through gproc with a
    %% particular channel name.
    try
        gproc:reg({p, l, {riak_pubsub_subscription, Channel}}),

        lager:warning("Registered ~p to send to ~p on channel ~p.\n",
                      [self(), Pid, Channel])
    catch
        _:_ ->
            lager:warning("Failed registration ~p to send to ~p on channel ~p.\n",
                          [self(), Pid, Channel]),

            {stop, registration_failed}
    end,

    {ok, #state{partition=Partition, channel=Channel, pid=Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({message, Message}, #state{channel=Channel, pid=Pid}=State) ->
    %% Relay information to the actual process which is subscribed.
    lager:warning("Received ~p for ~p ~p.\n",
                  [Message, Channel, Pid]),
    Pid ! Message,
    {noreply, State};
handle_info(Info, State) ->
    lager:warning("Received unknown message ~p\n.", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
