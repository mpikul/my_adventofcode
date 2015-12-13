%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_12).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
  day12_1/1, day12_2/1]).

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

day12_1(Path) ->
  gen_server:call(?SERVER, {day121, Path}, 300000).
day12_2(Path) ->
  gen_server:call(?SERVER, {day122, Path}, 300000).

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
handle_call({day121, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Data = unicode:characters_to_list(FileData),
  Res = get_numbers_sum(Data, 0),
  {reply, Res, State};

handle_call({day122, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Data = unicode:characters_to_list(FileData),
  Res = get_numbers_sum(Data, [value], 0),
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
get_numbers_sum([], Sum) ->
  Sum;

get_numbers_sum([_|T] = L, Sum) ->
  case string:to_integer(L) of
    {error, _} -> get_numbers_sum(T, Sum);
    {Val, Rest} -> get_numbers_sum(Rest, Sum + Val)
  end.

get_numbers_sum([], _, Sum) ->
  Sum;

get_numbers_sum([$"|T], [value|R], Sum) ->
  Idx = string:chr(T, $"),
  Value = string:left(T, Idx),
  get_numbers_sum(string:substr(T,Idx  + 1), get_state_for_string_value(Value, R), Sum);
get_numbers_sum([${|T], [value|R], Sum) ->
  get_numbers_sum(T, [{object, ok, Sum}] ++ R, Sum);
get_numbers_sum([$[|T], [value|R], Sum) ->
  get_numbers_sum(T, [list] ++ R, Sum);
get_numbers_sum(L, [value|R], Sum) ->
  {Val, Rest} = string:to_integer(L),
  get_numbers_sum(Rest, R, Sum + Val);

get_numbers_sum([$}|T], [{object, ok, _}|R], Sum) ->
  get_numbers_sum(T, R, Sum);
get_numbers_sum([$}|T], [{object, ignore, Sum}|R], _) ->
  get_numbers_sum(T, R, Sum);
get_numbers_sum([$,|T], [{object, State, SumBefore}|R], Sum) ->
  get_numbers_sum(T, [{object, State, SumBefore}|R], Sum);
get_numbers_sum([$"|T], [{object, _, _}|_] = S, Sum) ->
  get_numbers_sum(string:substr(T, string:str(T, "\":") + 2), [value] ++ S, Sum);

get_numbers_sum([$]|T], [list|R], Sum) ->
  get_numbers_sum(T, R, Sum);
get_numbers_sum([$,|T], [list|R], Sum) ->
  get_numbers_sum(T, [list|R], Sum);
get_numbers_sum(L, [list|_] = S, Sum) ->
  get_numbers_sum(L, [value] ++ S, Sum).

get_state_for_string_value("red\"", [{object, _, Sum}|R]) ->
  [{object, ignore, Sum}] ++ R;
get_state_for_string_value(_, State) ->
  State.

