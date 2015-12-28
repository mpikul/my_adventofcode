%%%-------------------------------------------------------------------
%%% @author mpikul
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2015 9:53 AM
%%%-------------------------------------------------------------------
-module(santa_19).
-author("mpikul").

-behaviour(gen_server).

%% API
-export([start_link/0,
         day19_1/1, day19_2/1]).

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

day19_1(Path) ->
  gen_server:call(?SERVER, {day191, Path}, 600000000).
day19_2(Path) ->
  gen_server:call(?SERVER, {day192, Path}, 600000000).

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
handle_call({day191, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  {Molecule, Replacements} = get_data(binary:split(FileData, [<<"\r\n">>], [global]), []),
  S = get_all_molecules(<<>>, Molecule, Replacements, sets:new()),
  {reply, sets:size(S), State};
handle_call({day192, Path}, _From, State) ->
  {ok, FileData} = file:read_file(Path),
  {Medicine, Replacements} = get_data(binary:split(FileData, [<<"\r\n">>], [global]), []),
  SortedReplacements  = lists:reverse(lists:sort(fun({_, Val1}, {_, Val2}) -> size(Val1) =< size(Val2) end, Replacements)),
  S = make_reduction({Medicine, Medicine, []}, SortedReplacements, SortedReplacements, sets:new(), []),
  {reply, {length(S), get_shortest_path_len(S)}, State};

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
get_data([L|T], Replacements) ->
  case parse_line(binary:split(L, <<" => ">>)) of
    {Name, Value} -> get_data(T, Replacements ++ [{Name, Value}]);
    continue -> get_data(T, Replacements);
    Molecule -> {Molecule, Replacements}
  end.

parse_line([Name, Val]) ->
  {Name, Val};
parse_line([<<>>]) ->
  continue;
parse_line([Molecule]) ->
  Molecule.

get_all_molecules(_, <<>>, _, ChildMolecules) ->
  ChildMolecules;
get_all_molecules(Prefix, Rest, Replacements, ChildMolecules) when size(Rest) =:= 1 ->
  get_all_molecules(<<Prefix/binary,Rest/binary>>, <<>>, Replacements, add_replacements(Prefix, Rest, Replacements, ChildMolecules));
get_all_molecules(Prefix, Rest, Replacements, ChildMolecules) when size(Rest) > 1 ->
  First = binary:part(Rest, {0, 1}),
  NewRest = binary:part(Rest, {1, size(Rest) - 1}),
  get_all_molecules(<<Prefix/binary,First/binary>>, NewRest, Replacements, add_replacements(Prefix, Rest, Replacements, ChildMolecules)).

add_replacements(_, _, [], ChildMolecules) ->
  ChildMolecules;
add_replacements(Prefix, Rest, [{Name, Value}|T], ChildMolecules) when size(Rest) > size(Name) ->
  NameLength = size(Name),
  add_replacements(Prefix, Rest, T, case binary:part(Rest, {0, NameLength}) =:= Name of
                                      true -> R = binary:part(Rest, {NameLength, size(Rest) - NameLength}),
                                              sets:add_element(<<Prefix/binary,Value/binary,R/binary>>, ChildMolecules);
                                      false -> ChildMolecules
                                    end);
add_replacements(Prefix, Name, [{Name, Value}|T], ChildMolecules) ->
  add_replacements(Prefix, Name, T, sets:add_element(<<Prefix/binary,Value/binary>>, ChildMolecules));
add_replacements(Prefix, Rest, [_|T], ChildMolecules) ->
  add_replacements(Prefix, Rest, T, ChildMolecules).

make_reduction({<<"e">>, _, CurrReductions}, _, _, _, Solutions) ->
  io:format("~p~n", [{length(CurrReductions), CurrReductions}]),
  Solutions ++ [CurrReductions];
make_reduction(_, [], _, _, Solutions) ->
  Solutions;
make_reduction({S, S, _} = Source, [{_, Value}|T] = R, AllReductions, Processed, Solutions) ->
  case sets:is_element(S, Processed) of
    true -> Solutions;
    false -> NewProcessed = sets:add_element(S, Processed),
             make_reduction(Source, T, AllReductions, Processed, perform_reduction(Source, R, binary:matches(S, Value), AllReductions, NewProcessed, Solutions))
  end;
make_reduction({S, _, _} = Source, [{_, Value}|T] = R, AllReductions, Processed, Solutions) ->
  case sets:is_element(S, Processed) of
    true -> Solutions;
    false -> NewProcessed = sets:add_element(S, Processed),
              make_reduction(Source, T, AllReductions, Processed, perform_reduction(Source, R, binary:matches(S, Value), AllReductions, NewProcessed, Solutions))
  end.

perform_reduction(_, [{_, _}|_], [], _, _, Solutions) ->
  Solutions;

perform_reduction({Source, FS, CurrReductions} = S, [{Name, Value}|Rest], [{0, _}|T], AllReplacements, Processed, Solutions) ->
  Suffix = binary:part(Source, {size(Value), size(Source) - size(Value)}),
  perform_reduction(S, [{Name, Value}|Rest], T, AllReplacements, Processed, make_reduction({<<Name/binary,Suffix/binary>>, FS, CurrReductions ++ [{Value, Name, 0, Source}]}, AllReplacements, AllReplacements, Processed, Solutions));
  %perform_reduction(S, [{Name, Value}|Rest], T, AllReplacements, Processed, make_reduction({<<Name/binary,Suffix/binary>>, FS, CurrReductions}, AllReplacements, AllReplacements, Processed, Solutions));

perform_reduction({Source, FS, CurrReductions} = S, [{Name, Value}|Rest], [{Start, _}|T], AllReplacements, Processed, Solutions) ->
  Prefix = binary:part(Source, {0, Start}),
  Suffix = binary:part(Source, {Start + size(Value), size(Source) - Start - size(Value)}),
  perform_reduction(S, [{Name, Value}|Rest], T, AllReplacements, Processed, make_reduction({<<Prefix/binary,Name/binary,Suffix/binary>>, FS, CurrReductions ++ [{Value, Name, Start, Source}]}, AllReplacements, AllReplacements, Processed, Solutions)).
  %perform_reduction(S, [{Name, Value}|Rest], T, AllReplacements, Processed, make_reduction({<<Prefix/binary,Name/binary,Suffix/binary>>, FS, CurrReductions}, AllReplacements, AllReplacements, Processed, Solutions)).


get_shortest_path_len([]) ->
  0;
get_shortest_path_len(S) ->
  [H|_] = lists:sort(fun(A, B) -> length(A) =< length(B) end, S),
  length(H).

%print_list([]) ->
%  ok;
%print_list([H|T]) ->
%  io:format("~p~n", [H]),
%  print_list(T).
