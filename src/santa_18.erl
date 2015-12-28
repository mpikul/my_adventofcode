%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_18).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day18_1/1, day18_2/1]).

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

day18_1(Path) ->
  gen_server:call(?SERVER, {day181, Path}, 600000).
day18_2(Path) ->
  gen_server:call(?SERVER, {day182, Path}, 600000).

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
handle_call({day181, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  LightsInfo = get_lights_info(binary:split(FileData, [<<"\r\n">>], [global]), {0, <<>>}),
  Res = update_lights(LightsInfo, fun perform_lights_update/4, 100),
  {reply, count_lights_on(Res, 0, 0), State};
handle_call({day182, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  LightsInfo = get_lights_info(binary:split(FileData, [<<"\r\n">>], [global]), {0, <<>>}),
  Res = update_lights(LightsInfo, fun perform_lights_update2/4, 100),
  {reply, count_lights_on(Res, 0, 0), State};

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
get_lights_info([], Acc) ->
  Acc;
get_lights_info([L|T], Acc) ->
  get_lights_info(T, read_lights_line(unicode:characters_to_list(L), Acc)).

read_lights_line([], {Dim, Binary}) ->
  {Dim + 1, Binary};
read_lights_line([$#|T], {Dim, Binary}) ->
  read_lights_line(T, {Dim, <<Binary/binary,1>>});
read_lights_line([$.|T], {Dim, Binary}) ->
  read_lights_line(T, {Dim, <<Binary/binary,0>>}).

update_lights(Lights, _, 0) ->
  Lights;
update_lights({Dim, Binary}, UpdateFun, Count) ->
  update_lights({Dim, UpdateFun(Binary, 0, Dim, <<>>)}, UpdateFun, Count - 1).

perform_lights_update(Binary, Index, _Dim, Acc) when Index =:= size(Binary) ->
  Acc;
perform_lights_update(Binary, Index, Dim, Acc) ->
  NewBinary = get_new_light_value(get_light_value(Binary, Index, 0, 0, Dim), get_neighbours_lights_on_count(Binary, Index, Dim)),
  perform_lights_update(Binary, Index + 1, Dim, <<Acc/binary,NewBinary/binary>>).

perform_lights_update2(Binary, Index, _Dim, Acc) when Index =:= size(Binary) ->
  Acc;
perform_lights_update2(Binary, 0, Dim, Acc) ->
  perform_lights_update2(Binary, 1, Dim, <<Acc/binary,<<1>>/binary>>);
perform_lights_update2(Binary, Idx, Dim, Acc) when Idx =:= (Dim - 1)->
  perform_lights_update2(Binary, Idx + 1, Dim, <<Acc/binary,<<1>>/binary>>);
perform_lights_update2(Binary, Idx, Dim, Acc) when Idx =:= ((Dim * Dim)- 1) ->
  perform_lights_update2(Binary, Idx + 1, Dim, <<Acc/binary,<<1>>/binary>>);
perform_lights_update2(Binary, Idx, Dim, Acc) when Idx =:= ((Dim * (Dim - 1))) ->
  perform_lights_update2(Binary, Idx + 1, Dim, <<Acc/binary,<<1>>/binary>>);
perform_lights_update2(Binary, Index, Dim, Acc) ->
  NewBinary = get_new_light_value(get_light_value(Binary, Index, 0, 0, Dim), get_neighbours_lights_on_count(Binary, Index, Dim)),
  perform_lights_update2(Binary, Index + 1, Dim, <<Acc/binary,NewBinary/binary>>).

get_new_light_value(1, N) when N =:= 2 orelse N =:= 3 ->
  <<1>>;
get_new_light_value(0, 3) ->
  <<1>>;
get_new_light_value(_, _) ->
  <<0>>.

get_neighbours_lights_on_count(Binary, Index, Dim) ->
  get_light_value(Binary, Index, -1, 0, Dim) + get_light_value(Binary, Index, 1, 0, Dim) +
  get_light_value(Binary, Index, 0, -1, Dim) + get_light_value(Binary, Index, 0, 1, Dim) +
  get_light_value(Binary, Index, -1, -1, Dim) + get_light_value(Binary, Index, -1, 1, Dim) +
  get_light_value(Binary, Index, 1, -1, Dim) + get_light_value(Binary, Index, 1, 1, Dim).

get_light_value(Binary, Index, X, Y, Dim) when (((Index + X) >= 0) andalso (((Index + X) div Dim) =:= (Index div Dim)) andalso
                                                ((Index + (Y * Dim)) >= 0) andalso (Index + (Y * Dim)) < size(Binary)) ->
  binary:at(Binary, Index + X + (Y *Dim));
get_light_value(_, _, _, _, _) ->
  0.

count_lights_on({_, Binary} = Info, Index, Acc) when Index < size(Binary)->
  NewAcc = case binary:at(Binary, Index) of
            1 -> Acc + 1;
             _ -> Acc
           end,
  count_lights_on(Info, Index + 1, NewAcc);
count_lights_on(_, _, Acc) ->
  Acc.