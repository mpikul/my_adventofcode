%%%-------------------------------------------------------------------
%%% @author RWH876
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_5).
-author("RWH876").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day5_1/1, day5_2/1]).

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

day5_1(Path) ->
  gen_server:call(?SERVER, {day51, Path}, 60000).
day5_2(Path) ->
  gen_server:call(?SERVER, {day52, Path}, 60000).

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
handle_call({day51, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Lines = binary:split(FileData, [<<"\r\n">>], [global]),
  Res = check_lines(Lines, fun check_line/1, 0),
  {reply, Res, State};
handle_call({day52, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Lines = binary:split(FileData, [<<"\r\n">>], [global]),
  Res = check_lines(Lines, fun check_line2/1, 0),
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
check_lines([], _FunCheckLine, Acc) ->
  Acc;
check_lines([L|T], FunCheckLine, Acc) ->
  NewAcc = case FunCheckLine(unicode:characters_to_list(L)) of
             true -> Acc + 1;
             false -> Acc
           end,
  check_lines(T, FunCheckLine, NewAcc).

check_line([]) ->
  false;
check_line(L) when length(L) < 3 ->
  false;

check_line(Line) ->
  Res1 = check_forbidden_string(Line, ["ab", "cd", "pq", "xy"]),
  Res2 = check_vowels(Line, "aeiou", 3, 0),
  Res3 = check_letters_in_row(Line, 2, ""),
  Res1 and Res2 and Res3.

check_line2([]) ->
  false;
check_line2(L) when length(L) < 3 ->
  false;

check_line2(Line) ->
  Res1 = check_pair_letters(Line, ""),
  Res2 = check_repeat_letters(Line, ""),
  Res1 and Res2.

check_forbidden_string(_Line, []) ->
  true;
check_forbidden_string(Line, [H|T]) ->
  case string:str(Line, H) of
    0 -> check_forbidden_string(Line, T);
    _ -> false
  end.

check_vowels([], _Vowels, Required, Current) ->
  Current >= Required;
check_vowels([H|T], Vowels, Required, Current) when Current < Required ->
  case string:chr(Vowels, H) of
    0 -> check_vowels(T, Vowels, Required, Current);
    _ -> check_vowels(T, Vowels, Required, Current + 1)
  end;
check_vowels(_Line, _Volwes, _Req, _Curr) ->
  true.

check_letters_in_row([], ReqLen, _Prefix) ->
  ReqLen =:= 0;
check_letters_in_row(_Line, 1, _Prefix) ->
  true;
check_letters_in_row([H|T], ReqLen, []) ->
  check_letters_in_row(T, ReqLen, [H]);
check_letters_in_row([H|T], ReqLen, [H|_] = Prefix) ->
  NewPrefix = Prefix ++ [H],
  case length(NewPrefix) >= ReqLen of
    true -> true;
    false -> check_letters_in_row(T, ReqLen, NewPrefix)
  end;
check_letters_in_row([H|T], ReqLen, _) ->
  check_letters_in_row(T, ReqLen, [H]).

check_repeat_letters([], SubStr) when length(SubStr) =:= 3 ->
  [C1, _, C3] = SubStr,
  C1 =:= C3;
check_repeat_letters([], _SubStr) ->
  false;
check_repeat_letters([H|T], SubStr) when length(SubStr) < 3 ->
  check_repeat_letters(T, SubStr ++ [H]);
check_repeat_letters(_,  [C1, _, C1]) ->
  true;
check_repeat_letters([H|T], [_, C2, C3]) ->
  check_repeat_letters(T, [C2, C3, H]).

check_pair_letters(Line, _Pair) when length(Line) < 2 ->
  false;
check_pair_letters([H|T], Pair) when length(Pair) < 2 ->
  check_pair_letters(T, Pair ++ [H]);
check_pair_letters([H|T] = Line, [_C1, C2] = Pair) ->
  case string:str(Line, Pair) of
    0 -> check_pair_letters(T, [C2, H]);
    _ -> true
  end.

