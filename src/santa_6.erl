%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_6).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day6_1/1, day6_2/1]).

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

day6_1(Path) ->
  gen_server:call(?SERVER, {day61, Path}, 240000).
day6_2(Path) ->
  gen_server:call(?SERVER, {day62, Path}, 240000).

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
handle_call({day61, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Lines = binary:split(FileData, [<<"\r\n">>], [global]),
  Res = check_lights(Lines, fun operate_lights_on_x/4, maps:new()),
  SizeList = [maps:size(X) || X <- maps:values(Res)],
  {reply, lists:sum(SizeList), State};
handle_call({day62, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Lines = binary:split(FileData, [<<"\r\n">>], [global]),
  Res = check_lights(Lines, fun operate_lights_on_x_2/4, maps:new()),
  SizeList = [lists:sum(Y) || Y <- [maps:values(X) || X <- maps:values(Res)]],
  {reply, lists:sum(SizeList), State};

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
check_lights([], _FunCheckLight, Acc) ->
  Acc;
check_lights([L|T], FunGenLight, Acc) ->
  NewAcc = check_light(string:tokens(unicode:characters_to_list(L), " "), FunGenLight, Acc),
  check_lights(T, FunGenLight, NewAcc).

check_light(["turn", Mode, From, "through", To], FunGenLight, LightsOn) ->
  {X1, Y1} = get_coord(From),
  {X2, Y2} = get_coord(To),
  generate_lights({min(X1, X2), min(Y1, Y2)}, {max(X1, X2), max(Y1, Y2)}, list_to_atom(Mode), FunGenLight, LightsOn);
check_light(["toggle", From, "through", To], FunGenLight, LightsOn) ->
  {X1, Y1} = get_coord(From),
  {X2, Y2} = get_coord(To),
  generate_lights({min(X1, X2), min(Y1, Y2)}, {max(X1, X2), max(Y1, Y2)}, list_to_atom("toggle"), FunGenLight, LightsOn);
check_light(_, _, LightsOn) ->
  LightsOn.

generate_lights({FromX, _FromY}, {ToX, _ToY}, _Mode, _FunGenLight, LightsOn) when FromX > ToX ->
  LightsOn;
generate_lights({FromX, FromY}, {ToX, ToY}, Mode, FunGenLight, LightsOn) ->
  XMap = case maps:find(FromX, LightsOn) of
           {ok, Map} -> Map;
           error -> maps:new()
         end,
  NewXMp = FunGenLight(FromY, ToY, Mode, XMap),
  generate_lights({FromX + 1, FromY}, {ToX, ToY}, Mode, FunGenLight, maps:put(FromX, NewXMp, LightsOn)).

operate_lights_on_x(FromY, ToY, toggle, LightsOn) when FromY =< ToY ->
  NewMap = case maps:find(FromY, LightsOn) of
             error -> maps:put(FromY, 1, LightsOn);
             {ok, _} -> maps:remove(FromY, LightsOn)
           end,
  operate_lights_on_x(FromY + 1, ToY, toggle, NewMap);
operate_lights_on_x(FromY, ToY, on, LightsOn) when FromY =< ToY ->
  NewMap = case maps:find(FromY, LightsOn) of
              error -> maps:put(FromY, 1, LightsOn);
              {ok, _} -> LightsOn
            end,
  operate_lights_on_x(FromY + 1, ToY, on, NewMap);
operate_lights_on_x(FromY, ToY, off, LightsOn) when FromY =< ToY ->
  operate_lights_on_x(FromY + 1, ToY, off, maps:remove(FromY, LightsOn));
operate_lights_on_x(_, _, _, LightsOn) ->
  LightsOn.

operate_lights_on_x_2(FromY, ToY, toggle, LightsOn) when FromY =< ToY ->
  NewMap = case maps:find(FromY, LightsOn) of
             error -> maps:put(FromY, 2, LightsOn);
             {ok, Val} -> maps:put(FromY, Val + 2, LightsOn)
           end,
  operate_lights_on_x_2(FromY + 1, ToY, toggle, NewMap);
operate_lights_on_x_2(FromY, ToY, on, LightsOn) when FromY =< ToY ->
  NewMap = case maps:find(FromY, LightsOn) of
             error -> maps:put(FromY, 1, LightsOn);
             {ok, Val} -> maps:put(FromY, Val + 1, LightsOn)
           end,
  operate_lights_on_x_2(FromY + 1, ToY, on, NewMap);
operate_lights_on_x_2(FromY, ToY, off, LightsOn) when FromY =< ToY ->
  NewMap = case maps:find(FromY, LightsOn) of
             error -> LightsOn;
             {ok, 1} -> maps:remove(FromY, LightsOn);
             {ok, Val} -> maps:put(FromY, Val - 1, LightsOn)
           end,
  operate_lights_on_x_2(FromY + 1, ToY, off, NewMap);
operate_lights_on_x_2(_, _, _, LightsOn) ->
  LightsOn.
get_coord(StrPos) ->
  [X,Y] = string:tokens(StrPos, ","),
  {list_to_integer(X), list_to_integer(Y)}.
