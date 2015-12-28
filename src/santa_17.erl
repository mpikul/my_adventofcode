%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_17).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day17_1/1, day17_2/1]).

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

day17_1(Path) ->
  gen_server:call(?SERVER, {day171, Path}, 600000).
day17_2(Path) ->
  gen_server:call(?SERVER, {day172, Path}, 600000).

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
handle_call({day171, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  ContainersInfo = get_containers_info(binary:split(FileData, [<<"\r\n">>], [global]), []),
  S = get_all_combinations(ContainersInfo, [], 150, []),
  {reply, length(S), State};
handle_call({day172, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  ContainersInfo = get_containers_info(binary:split(FileData, [<<"\r\n">>], [global]), []),
  S = get_all_combinations(ContainersInfo, [], 150, []),
  [H|_] = SS = lists:sort(fun (A, B) -> length(A) =< length(B) end, S),
  {reply, length(lists:takewhile(fun (E) -> length(E) =:= length(H) end, SS)), State};

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
get_containers_info([], Info) ->
  Info;
get_containers_info([[]|T], Info) ->
  get_containers_info(T, Info);
get_containers_info([L|T], Infos) when size(L) > 0 ->
  get_containers_info(T, Infos ++ [list_to_integer(unicode:characters_to_list(L))]);
get_containers_info([_L|T], Info) ->
  get_containers_info(T, Info).


get_all_combinations([H|T], CurrComb, Rest, Acc) when H < Rest ->
  NewAcc = get_all_combinations(T, CurrComb ++ [H], Rest - H, Acc),
  get_all_combinations(T, CurrComb, Rest, NewAcc);
get_all_combinations([H|T], CurrComb, H, Acc) ->
  get_all_combinations(T, CurrComb, H, Acc ++ [CurrComb ++ [H]]);
get_all_combinations([_|T], CurrComb, Rest, Acc) ->
  get_all_combinations(T, CurrComb, Rest, Acc);
get_all_combinations(_, _, _, Acc) ->
  Acc.
