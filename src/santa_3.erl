%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_3).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day3_1/1, day3_2/1]).

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

day3_1(Path) ->
  gen_server:call(?SERVER, {day31, Path}, 60000).
day3_2(Path) ->
  gen_server:call(?SERVER, {day32, Path}, 60000).

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
handle_call({day31, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Data = unicode:characters_to_list(FileData),
  Res = get_path(Data, {0,0}, [{{0,0}, 1}]),
  {reply, length(Res), State};
handle_call({day32, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  Data = unicode:characters_to_list(FileData),
  Res = get_path_w_rob(Data, {0,0}, {0,0}, santa, [{{0,0}, 1}]),
  {reply, length(Res), State};

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
get_path([], _P, L) ->
  L;
get_path([P|T], Pos, L) ->
  NewP = calculate_new_pos(P, Pos),
  get_path(T, NewP, store_pos(NewP, L)).

get_path_w_rob([], _PosS, _PosR, _Move, L) ->
  L;
get_path_w_rob([P|T], PosS, PosR, santa, L) ->
  NewP = calculate_new_pos(P, PosS),
  get_path_w_rob(T, NewP, PosR, robo, store_pos(NewP, L));
get_path_w_rob([P|T], PosS, PosR, robo, L) ->
  NewP = calculate_new_pos(P, PosR),
  get_path_w_rob(T, PosS, NewP, santa, store_pos(NewP, L)).

calculate_new_pos($<, {X,Y}) ->
  {X - 1,Y};
calculate_new_pos($>, {X,Y}) ->
  {X + 1,Y};
calculate_new_pos($v, {X,Y}) ->
  {X,Y - 1};
calculate_new_pos($^, {X,Y}) ->
  {X,Y + 1}.

store_pos(P, L) ->
  case lists:keyfind(P, 1, L) of
    {P, C} ->
      lists:keyreplace(P, 1, L, {P, C + 1});
    false ->
      lists:append(L, [{P, 1}])
  end.

