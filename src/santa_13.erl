%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_13).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day13_1/1, day13_2/1]).

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

day13_1(Path) ->
  gen_server:call(?SERVER, {day131, Path}, 240000).
day13_2(Path) ->
  gen_server:call(?SERVER, {day132, Path}, 240000).

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
handle_call({day131, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  {Persons, Happiness} = get_person_happiness(binary:split(FileData, [<<"\r\n">>], [global]), sets:new(), maps:new()),
  S = get_best_sort(perms(sets:to_list(Persons)), Happiness, max, none),
  {reply, S, State};
handle_call({day132, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  {Persons, Happiness} = get_person_happiness(binary:split(FileData, [<<"\r\n">>], [global]), sets:new(), maps:new()),
  S = get_best_sort(perms(sets:to_list(Persons)), Happiness, max, none),
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
get_person_happiness([], Persons, Happiness) ->
  {Persons, Happiness};
get_person_happiness([[]|T], Persons, Happiness) ->
  get_person_happiness(T, Persons, Happiness);
get_person_happiness([L|T], Persons, Happiness) when size(L) > 4 ->
  {Person, Level, Neighbour} = get_person_happiness_with_neighbour(string:tokens(unicode:characters_to_list(L), " ")),
  get_person_happiness(T, sets:add_element(Person, Persons), maps:put({Person, Neighbour}, Level, Happiness));
get_person_happiness([_L|T], Persons, Happiness) ->
  get_person_happiness(T, Persons, Happiness).

perms([]) ->
  [[]];
perms(L) ->
  [[H|T] || H <- L, T <- perms(L--[H])].

get_best_sort([], _Happiness, _Mode, Val) ->
  Val;
get_best_sort([L|T], Happiness, Mode, none) ->
  get_best_sort(T, Happiness, Mode, {calculate_happiness_level(L, none, none, Happiness, 0), L});
get_best_sort([L|T], Happiness, Mode, BestLevel) ->
  get_best_sort(T, Happiness, Mode, chose_sort(BestLevel, {calculate_happiness_level(L, none, none, Happiness, 0), L}, Mode)).

calculate_happiness_level([], First, Prev, Happiness, CurrLevel) ->
  CurrLevel +
  calculate_person_happiness_with_neighbour(First, Prev, Happiness) +
  calculate_person_happiness_with_neighbour(Prev, First, Happiness);
calculate_happiness_level([Person|T], none, none, Happiness, CurrLevel) ->
  calculate_happiness_level(T, Person, Person, Happiness, CurrLevel);
calculate_happiness_level([Person|T], First, Prev, Happiness, CurrLevel) ->
  calculate_happiness_level(T, First, Person, Happiness, CurrLevel +
                                                         calculate_person_happiness_with_neighbour(Person, Prev, Happiness) +
                                                         calculate_person_happiness_with_neighbour(Prev, Person, Happiness)).

chose_sort({D1, P1}, {error, _P2}, _Mode) ->
  {D1, P1};
chose_sort({D1, _P1}, {D2, P2}, min) when D2 < D1 ->
  {D2, P2};
chose_sort(Val, _, min) ->
  Val;
chose_sort({D1, _P1}, {D2, P2}, max) when D2 > D1 ->
  {D2, P2};
chose_sort(Val, _, max) ->
  Val.

get_person_happiness_with_neighbour([Person, "would", "gain", Level, "happiness", "units", "by", "sitting", "next", "to", Neighbour]) ->
  {Person, list_to_integer(Level), string:left(Neighbour, length(Neighbour) - 1)};
get_person_happiness_with_neighbour([Person, "would", "lose", Level, "happiness", "units", "by", "sitting", "next", "to", Neighbour]) ->
  {Person, -list_to_integer(Level), string:left(Neighbour, length(Neighbour) - 1)}.

calculate_person_happiness_with_neighbour(Person, Neighbour, Happiness) ->
  case maps:find({Person, Neighbour}, Happiness) of
    error -> error;
    {ok, Level} -> Level
  end.
