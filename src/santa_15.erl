%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_15).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day15_1/1, day15_2/1]).

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

day15_1(Path) ->
  gen_server:call(?SERVER, {day151, Path}, 600000).
day15_2(Path) ->
  gen_server:call(?SERVER, {day152, Path}, 600000).

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
handle_call({day151, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Ingredients = get_ingredients_info(binary:split(FileData, [<<"\r\n">>], [global]), []),
  S = get_best_cookie(generates_spoons_combinations(Ingredients, 0, 100, [], []), Ingredients, fun best_selector/2, {0, 0}),
  {reply, S, State};
handle_call({day152, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Ingredients = get_ingredients_info(binary:split(FileData, [<<"\r\n">>], [global]), []),
  S = get_best_cookie(generates_spoons_combinations(Ingredients, 0, 100, [], []), Ingredients, fun best_selector_w_cal/2, {0, 500}),
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
get_ingredients_info([], Info) ->
  Info;
get_ingredients_info([[]|T], Info) ->
  get_ingredients_info(T, Info);
get_ingredients_info([L|T], Infos) when size(L) > 4 ->
  Info = get_ingredients_info(string:tokens(unicode:characters_to_list(L), " :,")),
  get_ingredients_info(T, Infos ++ [Info]);
get_ingredients_info([_L|T], Info) ->
  get_ingredients_info(T, Info).

get_ingredients_info([Name, "capacity", Capacity, "durability",  Durability, "flavor", Flavor, "texture", Texture, "calories", Calories]) ->
  {Name, list_to_integer(Capacity), list_to_integer(Durability), list_to_integer(Flavor), list_to_integer(Texture), list_to_integer(Calories)}.

get_best_cookie([], _Ingredients, _BestSelector, Best) ->
  Best;
get_best_cookie([H|T], Ingredients, BestSelector, Best) ->
  get_best_cookie(T, Ingredients, BestSelector, BestSelector(Best, calculate_cookie(Ingredients, H))).

best_selector({S1, _} = B, {S2, _}) when S1 >= S2 ->
  B;
best_selector(_, B)  ->
  B.
best_selector_w_cal({S1, _}, {S2, C} = B) when (C =:= 500 andalso S2 > S1) ->
  B;
best_selector_w_cal(B, _)  ->
  B.

generates_spoons_combinations([_Last], _CurrIndex, TotalSpoons, CurrCombination, Acc) ->
  Acc ++ [CurrCombination ++ [TotalSpoons]];
generates_spoons_combinations([_|T] = Ingr, CurrIndex, TotalSpoons, CurrCombination, Acc) when  CurrIndex =< TotalSpoons ->
  NewAcc = generates_spoons_combinations(T, 0, TotalSpoons - CurrIndex, CurrCombination ++ [CurrIndex], Acc),
  generates_spoons_combinations(Ingr, CurrIndex + 1, TotalSpoons, CurrCombination, NewAcc);

generates_spoons_combinations(_, _, _, _, Acc) ->
  Acc.

calculate_cookie([], []) ->
  0;
calculate_cookie(Ingredients, Spoons) ->
  {TotalCapacity, TotalDurability, TotalFlavor, TotalTexture, TotalCalories} = calculate_properies(Ingredients, Spoons, {0, 0, 0, 0, 0}),
  {multiply_properties(TotalCapacity, multiply_properties(TotalDurability, multiply_properties(TotalFlavor, multiply_properties(TotalTexture, 1)))), TotalCalories}.

calculate_properies([],[], Total) ->
  Total;
calculate_properies([Ingredient|T], [Spoons|S], Total) ->
  calculate_properies(T, S, add_propery(Ingredient, Spoons, Total)).

add_propery({_, Capacity, Durability, Flavor, Texture, Calories}, IngredientSpoons, {TotalCapacity, TotalDurability, TotalFlavor, TotalTexture, TotalCalories}) ->
  {TotalCapacity + (IngredientSpoons * Capacity), TotalDurability + (IngredientSpoons * Durability), TotalFlavor + (IngredientSpoons * Flavor), TotalTexture + (IngredientSpoons * Texture), TotalCalories + (IngredientSpoons * Calories)}.

multiply_properties(Property, _Total) when Property < 0 ->
  0;
multiply_properties(Property, Total) ->
  Property * Total.
