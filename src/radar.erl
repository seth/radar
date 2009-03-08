%% @doc Radar, simple service discovery
%% @author Seth Falcon <seth@userprimary.net>
%% @copyright 2008-2009 Seth Falcon
%%%-------------------------------------------------------------------
-module(radar).
-vsn(1).
-author('seth@userprimary.net').
-behaviour(gen_server).
-import(urlutil, [parse_url/1, make_url/1]).
-import(hexutil, [to_hex_str/1]).
-import(erlang, [md5/1]).
-include_lib("stdlib/include/qlc.hrl").
%% API
-export([start_link/0, start/0, stop/0, test/0]).
-export([register/1, unregister/1, find/0, find/3]).
-export([make_service/4, service_url/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("service.hrl").

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

register(Service) when is_record(Service, service) ->
    gen_server:call(?MODULE, {register, Service}).

unregister(Service) when is_record(Service, service) ->
    gen_server:call(?MODULE, {unregister, Service}).

find() ->
    gen_server:call(?MODULE, {find, [], [], []}).

find(Type, Group, Attrs) ->
    gen_server:call(?MODULE, {find, Type, Group, Attrs}).

%% @spec make_service(Type, Group, Url, Attrs) -> #service{}
%% @doc Create a service record
make_service(Type, Group, Url, Attrs) ->
    UrlParts = parse_url(Url),
    case UrlParts of
        {Scheme, Host, Port, Path, _} ->
            Id = make_service_id(Type, Group, Url),
            #service{id=Id, type=Type, group=Group, proto=Scheme,
                    host=Host, port=Port, path=Path,
                     attrs=Attrs};
        {error, Reason, _} ->
            {error, bad_url, Reason, Url}
    end.

%% @spec service_url(Service) -> Url
%% @doc Return the URL of the specified service record.
service_url(Service) when is_record(Service, service) ->
    #service{proto=Scheme, host=Host, port=Port, path=Path} = Service,
    make_url({Scheme, Host, Port, Path, []}).

make_service_id(Type, Group, Url) ->
    to_hex_str(md5(lists:flatten([Type, Group, Url]))).

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
    radar_db:start(),
    {ok, []}.

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
    radar_db:add(Service),
    {reply, ok, State};
handle_call({unregister, Service}, _From, State) ->
    radar_db:remove(Service),
    {reply, ok, State};
handle_call({find, Type, Group, Attrs}, _From, State) ->
    Found = radar_db:find(Type, Group, Attrs),
    {reply, Found, State};
handle_call(stop, _From, State) ->
    radar_db:stop(),
    {stop, normal, stopped, State};
handle_call(Request, _From, State) ->
    Reply = {unknown_request, Request},
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
             
