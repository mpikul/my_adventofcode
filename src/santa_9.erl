%%%-------------------------------------------------------------------
%%% @author RWH876
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_9).
-author("RWH876").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day9_1/1, day9_2/1]).

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

day9_1(Path) ->
  gen_server:call(?SERVER, {day91, Path}, 240000).
day9_2(Path) ->
  gen_server:call(?SERVER, {day92, Path}, 240000).

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
handle_call({day91, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  {Cities, Distances} = get_city_distances(binary:split(FileData, [<<"\r\n">>], [global]), sets:new(), maps:new()),
  S = get_path(perms(sets:to_list(Cities)), Distances, min, {0, []}),
  {reply, S, State};
handle_call({day92, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  {Cities, Distances} = get_city_distances(binary:split(FileData, [<<"\r\n">>], [global]), sets:new(), maps:new()),
  S = get_path(perms(sets:to_list(Cities)), Distances, max, {0, []}),
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
get_city_distances([], Cities, Distances) ->
  {Cities, Distances};
get_city_distances([[]|T], Cities, Distances) ->
  get_city_distances(T, Cities, Distances);
get_city_distances([L|T], Cities, Distances) when size(L) > 4 ->
  [City1, "to", City2, "=", Dist] = string:tokens(unicode:characters_to_list(L), " "),
  {DistInt, _} = string:to_integer(Dist),
  get_city_distances(T, sets:add_element(City2, sets:add_element(City1, Cities)), maps:put({City2, City1}, DistInt,maps:put({City1, City2}, DistInt, Distances)));
get_city_distances([_L|T], Cities, Distances) ->
  get_city_distances(T, Cities, Distances).

perms([]) ->
  [[]];
perms(L) ->
  [[H|T] || H <- L, T <- perms(L--[H])].

get_path([], _Distances, _Mode, Val) ->
  Val;
get_path([L|T], Distances, Mode, {0, _}) ->
  get_path(T, Distances, Mode, {calculate_distance(L, none, Distances, 0), L});
get_path([L|T], Distances, Mode, DistPath) ->
  get_path(T, Distances, Mode, chose_path(DistPath, {calculate_distance(L, none, Distances, 0), L}, Mode)).

calculate_distance([], _Prev, _Distances, CurrDist) ->
  CurrDist;
calculate_distance([City|T], none, Distances, CurrDist) ->
  calculate_distance(T, City, Distances, CurrDist);
calculate_distance([City|T], Prev, Distances, CurrDist) ->
  case maps:find({Prev, City}, Distances) of
    error -> error;
    {ok, Dist} -> calculate_distance(T, City, Distances, CurrDist + Dist)
  end.

chose_path({D1, P1}, {error, _P2}, _Mode) ->
  {D1, P1};
chose_path({D1, _P1}, {D2, P2}, min) when D2 < D1 ->
  {D2, P2};
chose_path(Val, _, min) ->
  Val;
chose_path({D1, _P1}, {D2, P2}, max) when D2 > D1 ->
  {D2, P2};
chose_path(Val, _, max) ->
  Val.
