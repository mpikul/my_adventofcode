%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_14).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day14_1/1, day14_2/2]).

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

day14_1(Path) ->
  gen_server:call(?SERVER, {day141, Path}, 240000).
day14_2(Path, Time) ->
  gen_server:call(?SERVER, {day142, Path, Time}, 240000).

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
handle_call({day141, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Reindeers = get_reindeers_info(binary:split(FileData, [<<"\r\n">>], [global]), []),
  S = get_best_reinder(Reindeers, 2503, none),
  {reply, S, State};
handle_call({day142, Path, Time}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Reindeers = get_reindeers_info(binary:split(FileData, [<<"\r\n">>], [global]), []),
  ReindersWithStats = [{X, {0, 0}} || X <- Reindeers],
  S = get_best_reinder(ReindersWithStats, Time),
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
get_reindeers_info([], Info) ->
  Info;
get_reindeers_info([[]|T], Info) ->
  get_reindeers_info(T, Info);
get_reindeers_info([L|T], Info) when size(L) > 4 ->
  ReindeerInfo = get_reindeers_info(string:tokens(unicode:characters_to_list(L), " ")),
  get_reindeers_info(T, Info ++ [ReindeerInfo]);
get_reindeers_info([_L|T], Info) ->
  get_reindeers_info(T, Info).

get_best_reinder([], _Time, Best) ->
  Best;
get_best_reinder([{Name, _, _, _} = L|T], Time, none) ->
  get_best_reinder(T, Time, {Name, calculate_distance(L, Time)});
get_best_reinder([{Name, _, _, _} = L|T],Time, Best) ->
  get_best_reinder(T, Time, chose_better_reindeer({Name, calculate_distance(L, Time)}, Best, max)).

chose_better_reindeer({_, D1} = R1, {_, D2}, max) when D2 =< D1 ->
  R1;
chose_better_reindeer(_, R2, max) ->
  R2;
chose_better_reindeer({_, D1} = R1, {_, D2}, min) when D2 >= D1 ->
  R1;
chose_better_reindeer(_, R2, max) ->
  R2.

get_reindeers_info([Reindeer, "can", "fly", Speed, "km/s", "for", FlyTime, "seconds,", "but", "then", "must", "rest", "for", RestTime, "seconds."]) ->
  {Reindeer, list_to_integer(Speed), list_to_integer(FlyTime), list_to_integer(RestTime)}.

calculate_distance(_, 0) ->
  0;
calculate_distance({_, Speed, FlyTime, RestTime}, Time) ->
  CycleTime = FlyTime + RestTime,
  io:format("~p~n", [calculate_full_cycles_distance(Time div CycleTime, Speed, FlyTime) + calculate_rest_time_distance(Time rem CycleTime, Speed, FlyTime)]),
  calculate_full_cycles_distance(Time div CycleTime, Speed, FlyTime) + calculate_rest_time_distance(Time rem CycleTime, Speed, FlyTime).

calculate_full_cycles_distance(0, _, _) ->
  0;
calculate_full_cycles_distance(Cycles, Speed, FlyTime) ->
  Cycles * Speed * FlyTime.
calculate_rest_time_distance(0, _, _) ->
  0;
calculate_rest_time_distance(Time, Speed, FlyTime) when Time > FlyTime ->
  Speed * FlyTime;
calculate_rest_time_distance(Time, Speed, _) ->
  Speed * Time.

get_best_reinder(ReindersWithStats, Time) ->
  NewReindersWithStats = perform_moves(ReindersWithStats, 1, Time),
  lists:last(lists:sort(fun({_, {_,SA}}, {_, {_,SB}}) -> SA =< SB end, NewReindersWithStats)).

perform_moves(ReindersWithStats, Step, Time) when Step =< Time->
  [First|_] = NewReindersWithStats = lists:sort(fun({_, {DA,_}}, {_, {DB,_}}) -> DA >= DB end, [perform_single_move(X, Step) || X <- ReindersWithStats]),
  perform_moves(give_points(NewReindersWithStats, First, []), Step + 1, Time);
perform_moves(ReindersWithStats, _, _) ->
  ReindersWithStats.

give_points([], _Best, Acc) ->
  Acc;
give_points([{I, {D,S}}|T], {_, {D, _}} = Best, Acc) ->
  give_points(T, Best, Acc ++ [{I, {D,S + 1}}]);
give_points(L, _, Acc) ->
  Acc ++ L.

perform_single_move(S, 0) ->
  S;
perform_single_move({{_, _, FlyTime, RestTime}, _} = S, Time) when (Time rem (FlyTime + RestTime)) =:= 0 ->
  S;
perform_single_move({{_, Speed, FlyTime, RestTime} = Info, {Distance, Score}}, Time) when (Time rem (FlyTime + RestTime)) =< FlyTime ->
  {Info, {Distance + Speed, Score}};
perform_single_move(S, _) ->
  S.
