%%%-------------------------------------------------------------------
%%% @author RWH876
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_10).
-author("RWH876").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day10_1/2, day10_2/2]).

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

day10_1(Path, Number) ->
  gen_server:call(?SERVER, {day101, Path, Number}, 300000).
day10_2(Path, Number) ->
  gen_server:call(?SERVER, {day102, Path, Number}, 300000).

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
handle_call({day101, Path, Number}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Data = get_digits(unicode:characters_to_list(FileData), []),
  io:format("~p~n", [Data]),
  Res = look_and_say_many(Data, 1, Number),
  {reply, {length(Res), Res}, State};

handle_call({day102, Path, Number}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Data = get_digits(unicode:characters_to_list(FileData), []),
  Res = look_and_say_many(Data, 1, Number),
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
look_and_say([], none, Acc) ->
  binary_to_list(Acc);
look_and_say([], {C, N}, Acc) ->
  binary_to_list(<<Acc/binary,N/integer,C/integer>>);
look_and_say([C|T], {C, N}, Acc) ->
  look_and_say(T, {C, N + 1}, Acc);
look_and_say([C|T], none, Acc) ->
  look_and_say(T, {C, 1}, Acc);
look_and_say([C|T], {PC, N}, Acc) ->
  look_and_say(T, {C, 1}, <<Acc/binary,N/integer,PC/integer>>).

look_and_say_many(Val, From, To) when From =< To ->
  NewVal = look_and_say(Val, none, <<>>),
  look_and_say_many(NewVal, From + 1, To);
look_and_say_many(Val, _From, _To) ->
  Val.

get_digits([], Val) ->
  Val;
get_digits([L|T], Val) ->
  case string:to_integer([L]) of
    {error, _} -> get_digits(T, Val);
    {Digit, _} -> get_digits(T, Val ++ [Digit])
  end.

