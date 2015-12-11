%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_11).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day11_1/1, day11_2/1]).

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

day11_1(Path) ->
  gen_server:call(?SERVER, {day111, Path}, 300000).
day11_2(Path) ->
  gen_server:call(?SERVER, {day112, Path}, 300000).

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
handle_call({day111, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Data = unicode:characters_to_list(FileData),
  io:format("~p~n", [Data]),
  Res = get_next_password(Data),
  {reply, Res, State};

handle_call({day112, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Data = unicode:characters_to_list(FileData),
  io:format("~p~n", [Data]),
  Res = get_next_password(Data),
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
generate_next([], _, Acc) ->
  Acc;
generate_next([C|T], yes, Acc) ->
  generate_next(T, yes, Acc ++ [C]);
generate_next([$z|T], no, Acc) ->
  generate_next(T, no, Acc ++ [$a]);
generate_next([C|T], no, Acc) ->
  generate_next(T, yes, Acc ++ [C + 1]).

check_increasing([], _, _) ->
  false;
check_increasing([C|_T], Req, {CC, LC}) when (((C  - 1) =:= CC) and ((LC + 1) =:= Req)) ->
  true;
check_increasing([C|T], Req, {CC, Idx})  when ((C  - 1) =:= CC) ->
  check_increasing(T, Req, {C, Idx + 1});
check_increasing([C|T], Req, _) ->
  check_increasing(T, Req, {C, 1}).

check_forbidden(Pass, Forbidden) ->
  case string:tokens(Pass, Forbidden) of
    [Pass] -> true;
    _ -> false
  end.

check_pairs([], _) ->
  false;
check_pairs([C|T], C) ->
  check_pairs(T, {C, C});
check_pairs([C|T], {C, C}) ->
  check_pairs(T, {C, C});
check_pairs([C|_], {C, _}) ->
  true;
check_pairs([C|T], {_,O}) ->
  check_pairs(T, {C,O});
check_pairs([C|T], _) ->
  check_pairs(T, C).

check_password(Pass) ->
  check_forbidden(Pass, "iol") and check_increasing(Pass, 3, no) and check_pairs(Pass, empty).

get_next_password(Old) ->
  New = lists:reverse(generate_next(lists:reverse(Old), no, [])),
  case check_password(New) of
    true -> New;
    false -> get_next_password(New)
  end.
