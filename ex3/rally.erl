% Σταματελόπουλος Νικόλαος
% Α.Μ. : 03116138

-module(rally).
%-compile(export_all).

-export([rally/3]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MAX_SPEED, 24).

% definition of min integer function
-spec mymin(integer(), integer()) -> integer().
mymin(A, B) ->
    if
        A < B -> A;
        true -> B
    end.

%%%%%% Just a shuffle list function 
% http://erlang.org/pipermail/erlang-questions/2006-August/022198.html
shuffle(List) ->
    %% Determine the log n portion then randomize the list.
    randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
    randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) ->
			randomize(Acc)
		end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = lists:map(fun(A) -> 
			  {rand:uniform(), A}
		  end, List),

    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.

% make a list of all the units and their maximum speed limits
% list will be in reverse order but no need to fix that yet
% (we also divide limits by 10 for simplicity)
-spec make_track([{integer(), integer()}]) -> [integer()].
make_track(T) -> make_track(T, []).
make_track([{0,0}], Track) -> Track;
make_track([{0, _}|XS], Track) -> make_track(XS, Track);
make_track([{Tiles, Limit}|XS], Track) -> make_track([{Tiles-1, Limit}|XS], [Limit div 10|Track]).


%======================================================
% A SIMPLE RECURSIVE SOLUTION
%======================================================

% A simple function that moves N units in track
% checking if the move is valid (speed limits)
-spec perform_move([pos_integer()], integer()) -> error | [pos_integer()].
perform_move(Track, Speed) -> perform_move(Track, Speed, Speed).
perform_move([], _, _) -> [];
perform_move(Track, _, 0) -> Track;
perform_move([X|XS], Speed, N) ->
	if
		X < Speed -> error;
		true -> perform_move(XS, Speed, N-1)
	end.

% if you are able to reach speed 10 without violating any
% limits you should be able to finish safely
% check speed complexity is O(Speed^2)
-spec check_speed([pos_integer()], integer(), pos_integer()) -> boolean().
check_speed(_, Speed, _) when Speed < 2 -> true;
check_speed([], _, _) -> true;
check_speed(Track, Speed, B) ->
	case perform_move(Track, Speed) of
		error -> false;
		NewTrack -> check_speed(NewTrack, Speed-B, B)
	end.

% (Should never go below Speed-B)
% (A-B) iterations needed at most -> O((A-B)*(Speed+A)^2)
-spec find_max_possible_speed([pos_integer()], integer(), pos_integer(), pos_integer()) -> pos_integer().
find_max_possible_speed(Track, Speed, A, B) ->
	find_max_possible_speed(Track, Speed+A, B).
find_max_possible_speed(Track, Speed, B) ->
	case check_speed(Track, Speed, B) of
		true -> Speed;
		false -> find_max_possible_speed(Track, Speed-1, B)
	end.

% Counts min moves needed with greedy choise, the biggest
% possible speed that still does not violate any later
% speed limits
% O(Units * min(A+B, MAX_SPEED) * MAX_SPEED^2) with units being at most
% 10.000 and MAX_SPEED = 24, resulting in O(Units)
-spec race([pos_integer()], integer(), pos_integer(), pos_integer(), integer()) -> integer().
race([], _, _, _, Moves) -> Moves;
race(Track, Speed, A , B, Moves) ->
	NewSpeed = find_max_possible_speed(Track, Speed, A, B),
	NewTrack = perform_move(Track, NewSpeed),
	race(NewTrack, NewSpeed, A, B, Moves+1).


-spec rally([{pos_integer(), pos_integer()}], pos_integer(), pos_integer()) -> pos_integer().
rally(Acc, Br, List) ->
	%dp_rally(Acc, Br, List).
	A = Acc div 10,
	B = Br div 10,
	Track = lists:reverse([?MAX_SPEED|make_track(List, [])]),
	race(Track, 0, A, B, 0).


%======================================================
% Eunit tests to 'partially' check program
%======================================================

rally_test_() ->
	[
		test_make_track(),
		test_perform_move(),
		test_check_speed(),
		test_find_max_possible_speed(),
		test_rally()
	].

test_make_track() ->
	[
		?_assertEqual([10,10,10,10,10,10,10,10,10,10,7,7,7,7,7,4,4,4,10,10,10,10,10,10], lists:reverse(make_track([{10,100},{5,70},{3,40},{6,100},{0,0}]))),
		?_assertEqual([5,4,3,2,1,2,3,4,5] , lists:reverse(make_track([{1,50},{1,40},{1,30},{1,20},{1,10},{1,20},{1,30},{1,40},{1,50},{0,0}]))),
		?_assertEqual([10 || _ <- lists:seq(1,15)], make_track([{15,100},{0,0}]))
	].

test_perform_move() ->
	[
		?_assertEqual(error, perform_move([1,1,1], 7)),
		?_assertEqual([4,4,4], perform_move([2,3,4,4,4], 2))
	].

test_check_speed() ->
	[
		?_assertEqual(true, check_speed([4,4,4,4,2,2,1], 4, 2)),
		?_assertEqual(false, check_speed([4,4,3,4,2,2,1], 4, 2))
	].

test_find_max_possible_speed() ->
	[
		?_assertEqual(3, find_max_possible_speed([10,10,10,10,10,10,10,10,10,10,7,7,7,7,7,4,4,4,10,10,10,10,10,10], 0, 3, 1)),
		?_assertEqual(6, find_max_possible_speed([10,10,10,10,10,10,10,7,7,7,7,7,4,4,4,10,10,10,10,10,10], 3, 3, 1)),
		?_assertEqual(5, find_max_possible_speed([10,7,7,7,7,7,4,4,4,10,10,10,10,10,10], 6, 3, 1)),
		?_assertEqual(4, find_max_possible_speed([7,4,4,4,10,10,10,10,10,10], 5, 3, 1)),
		?_assertEqual(7, find_max_possible_speed([10,10,10,10,10,10], 4, 3, 1))
	].

test_rally() ->
	[
		?_assertEqual(5, rally(30,10,[{10,100},{5,70},{3,40},{6,100},{0,0}])),
		?_assertEqual(3, rally(40,50,[{15,100},{0,0}])),
		?_assertEqual(5, rally(40,20, [{1,50},{1,40},{1,30},{1,20},{1,10},{1,20},{1,30},{1,40},{1,50},{0,0}])),
		?_assertEqual(2, rally(40,40, [{1,10},{0,0}]))
	].

%======================================================
% Some type generators for property based testing
%======================================================

gen_speed() ->
	?LET(S, range(1,24), S*10).

gen_track_tuple() ->
	union([
		{pos_integer(), gen_speed()}
	]).

gen_random_track_N_tuples(N) ->
	?LET(L, vector(N, gen_track_tuple()), lists:append(L,[{0,0}])).

gen_random_track() ->
	?LET(N, pos_integer(), gen_random_track_N_tuples(N)).

%%%%%
gen_random_track_list_N(N) ->
	?LET(V, vector(N,range(1,24)), V).

gen_random_track_list() ->
	?LET(N, range(1,10000), gen_random_track_list_N(N)).

%%%%%%
gen_valid_move() ->
	?LET(Speed, range(1,24), {list(range(Speed,24)), Speed}).

%%%%%%
gen_invalid_track_for_speed(Speed) ->
	?LET(L, vector(Speed-1, range(Speed, 24)), shuffle([Speed-1|L])).

%%%%%%
gen_perfom_move_fail() ->
	?LET(S, range(1,24), {gen_invalid_track_for_speed(S), S}).

%%%%%%
gen_max_possible_speed_prop_conditions() ->
	union([{gen_random_track_list(),range(1,24),range(1,24),range(1,24)}]).

%%%%%%
gen_rally_input() ->
	union([{gen_random_track(),gen_speed(),gen_speed()}]).


%======================================================
% Property based tests to check program's correctness
%======================================================

% Something for make_track(List)
prop_track_length() ->
	?FORALL(Track, gen_random_track(), lists:foldl(fun({X,_}, Sum) -> X+Sum end, 0, Track) =:= length(make_track(Track))).

%------------------------------------------------------

% Something for performe_move(Track, Speed)
prop_perform_move_length() ->
	?FORALL({Track, Speed}, union([{gen_random_track_list(),range(1,24)}]), perform_move(Track, Speed)=:=error orelse length(perform_move(Track, Speed))=:=length(Track)-Speed orelse perform_move(Track, Speed)=:=[]).

prop_check_perform_move_success() ->
	?FORALL({Track, Speed}, gen_valid_move(), length(perform_move(Track, Speed))=:=length(Track)-Speed orelse perform_move(Track, Speed)=:=[]).

prop_check_perform_move_fail() ->
	?FORALL({Track, Speed}, gen_perfom_move_fail(), perform_move(Track, Speed)=:=error).

%------------------------------------------------------

% Something for check_speed(Track, Speed, B)
% Out of ideas or time or both. [|:-/

%------------------------------------------------------

% Something for find_max_possible_speed(Track, Speed, A, B)
prop_max_possible_speed()->
	?FORALL({Track, Speed, A, B}, gen_max_possible_speed_prop_conditions(), ?IMPLIES(check_speed(Track,Speed-B,B),
		find_max_possible_speed(Track, Speed, A, B)>=Speed-B
		andalso
		find_max_possible_speed(Track, Speed, A, B)=<Speed+A
		andalso
		check_speed(Track, find_max_possible_speed(Track, Speed, A, B),B)=:=true
		andalso
		(
			check_speed(Track,find_max_possible_speed(Track, Speed, A, B)+1,B)/=true
			orelse
			find_max_possible_speed(Track, Speed, A, B)+1>Speed+A
		))
	).

%------------------------------------------------------

% Something for race(Track, Speed, A, B)
% Not needed as it just calls 3 other functions
% (has no independent roll in program)

%------------------------------------------------------

% Checking that both greedy step of max possible speed
% and exhaustive search with dp algorithm give same result
prop_check_rally() ->
	?FORALL({Track,A,B}, gen_rally_input(), rally(A,B,Track)=:=dp_rally(A,B,Track)).



%======================================================
% SOLUTION WITH DP PROGRAMMING ALGORITHM
%======================================================
% dp matrix 24x24 (MAX_SPEED = 24)
% DP[i][j] = Minimum Moves needed to reach end of track
% starting from ((Number of units scanned*)-i) units from
% the end with previous speed j
% Starts from the end of the track at the last {0,0} unit
% and continues scanning towards the start of the track
% starting shape (also holds minLimit in tuple from last
% scanned unit to the target, and speedindex starts form 1)
% DP[i][j] = min(DP[i+j+k][j+k]) with -Brake<=k<=Acceleeration
% and 0<j+k<=min(limits,MAX_SPEED) else if not possible then 10002
% DP = [
% 		{24, [0, 0, 0, 0,...... ,0, 0, 0, 0, 0]},  <-(starts at {0,0})
% 		{24,    [0, 0, 0,...... ,0, 0, 0, 0, 0]},
% 		{24,       [0, 0,...... ,0, 0, 0, 0, 0]},
%       	           ..........
%   	{24,                                [0]}
% 	   ]
% Results in O(Units*MAX_SPEED^2) but copying the matrix
% through different functions degrades performance and simpler
% solution seems to perform better
-type dp_matrix() :: [{pos_integer(), [integer()]}].

-spec init_dp() -> dp_matrix().
init_dp() -> lists:reverse([{?MAX_SPEED,[0 || _ <- lists:seq(1,X)]} || X <- lists:seq(1,?MAX_SPEED)]).

-spec get_diagonal(dp_matrix(), [integer()]) -> [{integer(), integer()}].
get_diagonal([], Acc) -> lists:reverse(Acc);
get_diagonal([{Limit,[X|_]}|LS], Acc) -> get_diagonal(LS, [{Limit,X}|Acc]).

% Compare possible steps for Starting Point/Speed
% and return best moves that achieve it if possible
-spec helper_min_moves(pos_integer(), pos_integer(), integer(), integer(), integer(), [{integer(), integer()}]) -> integer().
helper_min_moves(_,_,X,_,_, []) -> X;
helper_min_moves(Acc, Br, X, StartSpeed, EndSpeed, [{L,Y}|YS]) ->
	SpeedChange = EndSpeed - StartSpeed,
	if
		EndSpeed > L orelse SpeedChange < -Br orelse SpeedChange > Acc -> NewX = X;
		true -> NewX = mymin(X, Y+1)
	end,
	helper_min_moves(Acc, Br, NewX, StartSpeed, EndSpeed+1, YS).

% Find minimum moves to end from given starting point for all possible starting speeds
-spec compute_moves(pos_integer(), pos_integer(), integer(), [{integer(), integer()}], [integer()]) -> [integer()].
compute_moves(_, _, 0, _, Acc) -> Acc;
compute_moves(A, B, StartSpeed, Y, Acc) ->
	Moves = helper_min_moves(A, B, 10002, StartSpeed, 1, Y),
	compute_moves(A, B, StartSpeed-1, Y, [Moves|Acc]).

-spec commit_step_to_matrix(dp_matrix(), integer(), dp_matrix()) -> dp_matrix().
commit_step_to_matrix([{_,[_]}], _, Acc) -> lists:reverse(Acc);
commit_step_to_matrix([{L,[_|XS]}|YS], L_, Acc) ->
	NewLimit = mymin(L,L_),
	commit_step_to_matrix(YS, L_, [{NewLimit,XS}|Acc]).

-spec make_a_step(pos_integer(), pos_integer(), pos_integer(), dp_matrix()) -> dp_matrix().
make_a_step(X, A, B, DP_matrix) ->
	Diag = get_diagonal(DP_matrix, []),
	NewMoves = compute_moves(A, B, ?MAX_SPEED, Diag, []),
	NewDP = commit_step_to_matrix(DP_matrix, X, []),
	[{X, NewMoves}|NewDP].

-spec last_step(pos_integer(), integer(), [{integer(), integer()}], integer()) -> integer().
last_step(_, _, [], MinMoves) -> MinMoves;
last_step(A, EndSpeed, [{L,Y}|YS], MinMoves) ->
	if
		EndSpeed > L orelse EndSpeed > A -> MinMoves;
		true -> last_step(A, EndSpeed+1, YS, mymin(MinMoves, Y+1))
	end.

-spec dp_race(pos_integer(), pos_integer(), [pos_integer()], dp_matrix()) -> integer().
dp_race(A, _, [], DP_matrix) -> 
	Diag = get_diagonal(DP_matrix, []),
	last_step(A, 1, Diag, 10002);
dp_race(A, B, [U|US], DP_matrix) ->
	DP_ = make_a_step(U, A, B, DP_matrix),
	dp_race(A, B, US, DP_).

-spec dp_rally(pos_integer(), pos_integer(), [{integer(), integer()}]) -> integer().
dp_rally(A, B, List) ->
	Track = make_track(List, []),
	DP = init_dp(),
	dp_race(A div 10, B div 10, Track, DP).