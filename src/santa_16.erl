%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_16).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day16_1/2, day16_2/2]).

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

day16_1(Path1, Path2) ->
  gen_server:call(?SERVER, {day161, Path1, Path2}, 600000).
day16_2(Path1, Path2) ->
  gen_server:call(?SERVER, {day162, Path1, Path2}, 600000).

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
handle_call({day161, AuntsPath, SearchPath}, _From, State) ->
  {ok, FileData} = file:read_file(AuntsPath),
  {ok, FileData2} = file:read_file(SearchPath),
  AuntsInfo = get_info(binary:split(FileData, [<<"\r\n">>], [global]), fun get_aunts_info/1, []),
  SearchInfo = get_info(binary:split(FileData2, [<<"\r\n">>], [global]), fun get_search_info/1, []),
  S = get_aunt(AuntsInfo, fun check_aunt1/2, SearchInfo),
  {reply, S, State};
handle_call({day162, AuntsPath, SearchPath}, _From, State) ->
  {ok, FileData} = file:read_file(AuntsPath),
  {ok, FileData2} = file:read_file(SearchPath),
  AuntsInfo = get_info(binary:split(FileData, [<<"\r\n">>], [global]), fun get_aunts_info/1, []),
  SearchInfo = get_info(binary:split(FileData2, [<<"\r\n">>], [global]), fun get_search_info/1, []),
  S = get_aunt(AuntsInfo, fun check_aunt2/2, SearchInfo),
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
get_info([], _InfoCallback, Info) ->
  Info;
get_info([[]|T], InfoCallback, Info) ->
  get_info(T, InfoCallback, Info);
get_info([L|T], InfoCallback, Infos) when size(L) > 4 ->
  Info = InfoCallback(string:tokens(unicode:characters_to_list(L), " :,")),
  get_info(T, InfoCallback, Infos ++ [Info]);
get_info([_L|T], InfoCallback, Info) ->
  get_info(T, InfoCallback, Info).

get_aunts_info(["Sue", Id, Key1, Val1, Key2,  Val2, Key3, Val3]) ->
  [{"Id", list_to_integer(Id)}, {Key1, list_to_integer(Val1)}, {Key2, list_to_integer(Val2)}, {Key3, list_to_integer(Val3)}].
get_search_info([Key, Val]) ->
  {Key, list_to_integer(Val)}.

get_aunt([], _CheckMethod, _Search) ->
  none;
get_aunt([H|T], CheckMethod, Search) ->
  case CheckMethod(Search, H) of
    true -> H;
    false -> get_aunt(T, CheckMethod, Search)
  end.

check_aunt1([], _) ->
  true;
check_aunt1([{Key, Value}|T], AuntInfo) ->
  case proplists:get_value(Key, AuntInfo) of
    Value -> check_aunt1(T, AuntInfo);
    undefined -> check_aunt1(T, AuntInfo);
    _ -> false
  end.

check_aunt2([], _) ->
  true;
check_aunt2([{"cats", _}|_] = S, AuntInfo) ->
  check_greater(S, AuntInfo);
check_aunt2([{"trees", _}|_] = S, AuntInfo) ->
  check_greater(S, AuntInfo);
check_aunt2([{"pomeranians", _}|_] = S, AuntInfo) ->
  check_fewer(S, AuntInfo);
check_aunt2([{"goldfish", _}|_] = S, AuntInfo) ->
  check_fewer(S, AuntInfo);
check_aunt2([{Key, Value}|T], AuntInfo) ->
  case proplists:get_value(Key, AuntInfo) of
    Value -> check_aunt2(T, AuntInfo);
    undefined -> check_aunt2(T, AuntInfo);
    _ -> false
  end.

check_greater([{Key, Value}|T], AuntInfo) ->
  case proplists:get_value(Key, AuntInfo) of
    undefined -> check_aunt2(T, AuntInfo);
    InfoValue -> ((InfoValue > Value) andalso check_aunt2(T, AuntInfo))
  end.
check_fewer([{Key, Value}|T], AuntInfo) ->
  case proplists:get_value(Key, AuntInfo) of
    undefined -> check_aunt2(T, AuntInfo);
    InfoValue -> ((InfoValue < Value) andalso check_aunt2(T, AuntInfo))
  end.

