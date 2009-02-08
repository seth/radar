%%%-------------------------------------------------------------------
%%% File    : radar.erl
%%% Author  : Seth Falcon <seth@userprimary.net>
%%% Description : 
%%%
%%% Created : 17 Dec 2008 by Seth Falcon <seth@userprimary.net>
%%%-------------------------------------------------------------------
-module(radar).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0, register/1, find/1, test/0]).
-export([setup_disk_db/0, init_db/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(service, {id = "",
                  type = "",
                  group = "",
                  proto = "http",
                  host = "",
                  port = 80,
                  path = "",
                  attrs = [],
                  expiry = 0,
                  lease = 300}).
-record(state, {services = []}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    start_link().
stop() ->
    gen_server:call(?MODULE, stop).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(Service) ->
    gen_server:call(?MODULE, {register, Service}).

find(Name) ->
    gen_server:call(?MODULE, {find, Name}).

setup_disk_db() ->
    Result = mnesia:create_schema([node()]),
    {_, {_, {Status,_}}} = Result,
    case Status of
        already_exists ->
            {ok, "mnesia already initialized", node()};
        ok ->
            {ok, "mnesia initialized", node()};
        _ -> {error, Result, node()}
    end.

init_db() ->
    mnesia:start(),
    mnesia:create_table(services,
                        [{attributes, record_info(fields, service)}]).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    init_db(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({register, Service}, _From, State) ->
    State2 = #state{services = [Service | State#state.services]},
    {reply, ok, State2};
handle_call({find, Name}, _From, State) ->
    Found = lists:keysearch(Name, 1, State#state.services),
    Reply = case Found of
                {value, Service} -> Service;
                false -> false
            end,
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

test() ->
    io:format("ok~n"),
    ok.
