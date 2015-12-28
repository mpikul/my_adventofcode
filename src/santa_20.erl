%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_20).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day20_1/1, day20_2/1]).

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

day20_1(Path) ->
  gen_server:call(?SERVER, {day201, Path}, 600000000).
day20_2(Path) ->
  gen_server:call(?SERVER, {day202, Path}, 600000000).

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
handle_call({day201, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Presents = list_to_integer(unicode:characters_to_list(FileData)),
  S = get_number(Presents),
  {reply, lists:last(S), State};
handle_call({day202, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Presents = list_to_integer(unicode:characters_to_list(FileData)),
  S = get_number2(Presents),
  {reply, S, State};

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
get_number(N) ->
  Val = N div 10,
  get_smallest(4, Val, []).

get_number2(N) ->
  get_smallest2(4, N, []).

get_smallest(N, N, Acc) ->
  Acc;
get_smallest(Val, Sum, Acc) ->
  case Val rem 10000 of
    0 -> io:format("~p~n", [Val]);
    _ -> ok
  end,
  Divs = get_div(2, Val div 2, Val,  [1, Val]),
  CurrSum = lists:sum(Divs),
  case CurrSum >= Sum of
    true -> Acc ++ [Val];
    false -> get_smallest(Val + 1, Sum, Acc)
  end.

get_smallest2(N, N, Acc) ->
  Acc;
get_smallest2(Val, Sum, Acc) ->
  case Val rem 10000 of
    0 -> io:format("~p~n", [Val]);
    _ -> ok
  end,
  Divs = get_div(2, Val div 2, Val,  [1, Val]),
  CurrSum = lists:sum(lists:filter(fun(E) -> E * 50 >= Val end, Divs)) * 11,
  case CurrSum >= Sum of
    true -> Acc ++ [Val];
    false -> get_smallest(Val + 1, Sum, Acc)
  end.

get_div(From, From, N, Acc) when (N rem From) =:= 0 ->
  Acc ++ [From];
get_div(From, From, _, Acc) ->
  Acc;

get_div(From, To, N, Acc) when (N rem From) =:= 0 ->
  get_div(From + 1, To, N, Acc ++ [From]);
get_div(From, To, N, Acc) ->
  get_div(From + 1, To, N, Acc).

