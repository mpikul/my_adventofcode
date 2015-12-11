%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_7).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day7_1/1, day7_2/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

day7_1(Path) ->
  gen_server:call(?SERVER, {day71, Path}, 240000).
day7_2(Path) ->
  gen_server:call(?SERVER, {day72, Path}, 240000).

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
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                   {reply, Reply :: term(), NewState :: #state{}} |
                   {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                   {noreply, NewState :: #state{}} |
                   {noreply, NewState :: #state{}, timeout() | hibernate} |
                   {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                   {stop, Reason :: term(), NewState :: #state{}}).
handle_call({day71, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Instructions = get_instructions(binary:split(FileData, [<<"\r\n">>], [global]), []),
  Res = get_wire_signal(Instructions, Instructions, maps:new(), "a"),
  {reply, Res, State};
handle_call({day72, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Instructions = get_instructions(binary:split(FileData, [<<"\r\n">>], [global]), []),
  Res = get_wire_signal(Instructions, Instructions, maps:new(), "a"),
  {reply, Res, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                   {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_instructions([], Acc) ->
  Acc;
get_instructions([L|T], Acc) ->
  get_instructions(T, lists:append(Acc, [string:tokens(unicode:characters_to_list(L), " ")])).

get_wire_signal(Wire, All, Map) ->
  case maps:find(Wire, Map) of
    {ok, Value} -> {Value, Map};
    error ->  case string:to_integer(Wire) of
                {error,no_integer} -> {Res, NewMap} = get_wire_signal(All, All, Map, Wire),
                  {Res, maps:put(Wire, Res, NewMap)};
                {Value, _} ->  {Value, Map}
              end
  end.

get_wire_signal([], _All, _Map, Wire) ->
  list_to_integer(Wire);

get_wire_signal([[W1, Op, W2, "->", Wire]|_], All, Map, Wire) ->
  {Val1, NewMap1} = get_wire_signal(W1, All, Map),
  {Val2, NewMap2} = get_wire_signal(W2, All, NewMap1),
  {calculate_signal(Val1, Op, Val2), NewMap2};

get_wire_signal([[Op, W, "->", Wire]|_], All, Map, Wire) ->
  {Val, NewMap} = get_wire_signal(W, All, Map),
  {calculate_signal(Op, Val), NewMap};

get_wire_signal([[Val, "->", Wire]|_], All, Map, Wire) ->
  get_wire_signal(Val, All, Map);

get_wire_signal([_|T], All, Map, Wire) ->
  get_wire_signal(T, All, Map, Wire).

calculate_signal(Val1, "AND", Val2) ->
  Val1 band Val2;
calculate_signal(Val1, "OR", Val2) ->
  Val1 bor Val2;
calculate_signal(Val1, "LSHIFT", Val2) ->
  Val1 bsl Val2;
calculate_signal(Val1, "RSHIFT", Val2) ->
  Val1 bsr Val2.
calculate_signal("NOT", Val) ->
  bnot Val.
