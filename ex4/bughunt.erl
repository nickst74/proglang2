% Σταματελόπουλος Νικόλαος
% Α.Μ. : 03116138

% 49/50 implementation where found to have bugs
% vector_42 seems to be the correct one
% we start from more general properties trying to express
% accurately the wrong parts and move on with more general
% properties until we perform an evaluation comparison on
% random inputs with our own vector evaluator

-module(bughunt).

-export[test/1, test_all/0].
%-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


-type vector() :: vectors:vector().
-type expr() :: vectors:expr().
-type int_expr() :: vectors:int_expr().
%-type vector_op() :: vectors:vector_op().
%-type scalar_op() :: vectors:scalar_op().
%-type norm_op() :: vectors:norm_op().

% Some custom types to use when specifying function types
-type eval_result() :: vector() | error.
-type result_or_exception() :: eval_result() | error:exception().
-type test_result() :: correct | {expr(), eval_result(), result_or_exception(), string()}.
-type prop_type() :: {proper:outer_test(), pos_integer(), string()}.

-type evaluator() :: fun((expr()) -> eval_result()).


% A helper function to run all 50 evaluators and print correct
% or the reason it failed accordingly
-spec test_all() -> ok.
test_all() ->
    test_all(1, 0).
test_all(N, Sum) when N > 50 -> io:fwrite("~w/50 are bugged~n", [Sum]), ok;
test_all(N, Sum) ->
    Result = test(N),
    case Result of
        correct -> M = correct, NSum = Sum;
        _ -> {_, _, _, M} = Result, NSum = Sum+1
    end,
    io:fwrite("~w: ~s~n", [N, M]),
    test_all(N+1, NSum).

% runs all Property Based Tests on an evaluator
% (specified by the given Id)
-spec test(pos_integer()) -> test_result().
test(VectorId) ->
    % get vector evaluator implementation for testing
    Vector = vectors:vector(VectorId),
    % a list of the properties to use for testing
    % (start from specific and continue to more general and arithmetic ones)
    Props = [
        % First the operation support checking
        {prop_check_add(Vector), 1, "The operation 'add' is not supported."},
        {prop_check_sub(Vector), 1, "The operation 'sub' is not supported."},
        {prop_check_dot(Vector), 1, "The operation 'dot' is not supported."},
        {prop_check_mul(Vector), 1, "The operation 'mul' is not supported."},
        {prop_check_div(Vector), 1, "The operation 'div' is not supported."},
        {prop_check_norm_one(Vector), 1, "The operation 'norm_one' is not supported."},
        {prop_check_norm_inf(Vector), 1, "The operation 'norm_inf' is not supported."},
        % Check division by zero
        {prop_check_zero_div(Vector), 1, "Not failing when dividing by zero."},
        % Check vector supported dimensions and their evaluation
        {prop_check_empty_vector(Vector), 1, "Not failing on empty vector expression."},
        {prop_check_long_vector(Vector), 10, "Not failing on vector with more than 100 elements."},
        {prop_check_single_element(Vector), 10, "Single element vectors not supported."},
        {prop_check_vector_eval(Vector), 20, "Fails to correctly evaluate vectors."},
        % Vector operation with different vector dimensions
        {prop_check_arg_length(Vector), 10, "Vector operation not failing for different argument lengths."},
        % Check nesting for different operations
        {prop_check_nesting_support(Vector), 10, "Nested operations not supported."},
        % Exactly 100 times nested expressions should succeed
        {prop_100nested_vector_op(Vector), 20, "Failing on 100 times nested vector operations."},
        {prop_100nested_scalar_op(Vector), 20, "Failing on 100 times nested scalar operations."},
        % More than 100 times nested expressions should fail
        {prop_deep_vector_op(Vector), 50, "Not failing on deeply nested vector operations."},
        {prop_deep_scalar_op(Vector), 50, "Not failing on deeply nested scalar operations."},
        % Last and more general property just compares with my implementation
        {prop_general_arithmetic_comprarison(Vector), 200, "Wrong output found."}
    ],
    test_properties(Vector, Props).


%test given properties on evaluator
-spec test_properties(evaluator(), [prop_type()]) -> test_result().
test_properties(_, []) -> correct;
test_properties(Vector, [{Prop, N, Message}|XS]) ->
    % run property with quickcheck for N times and use quiet option
    % so the output in terminal is more clear
    case proper:quickcheck(Prop, [N, quiet]) of
        true ->
            %property satisfied, check next
            test_properties(Vector, XS);
        false ->
            %property failed return Details in format:
            % {InputExpression, Expected Output, ActualOutput, FaultMessage}
            Input = hd(proper:counterexample()),
            try
                {Input, eval_expr(Input), Vector(Input), Message}
            catch
                error:Exception -> {Input, eval_expr(Input), Exception, "Raised Exception"}
            end
        end.


%===========================================================
%Properties' definitions
%===========================================================

% SIMPLE PROPERTIES TO CHECK IF ALL OPERATIONS ARE SUPPORTED
prop_check_add(Vector) ->
    %generate input type
    InputType = union([{add, [1], [1]}]),
    ?FORALL(Input, InputType, Vector(Input) /= error).

prop_check_sub(Vector) ->
    %generate input type
    InputType = union([{sub, [1], [1]}]),
    ?FORALL(Input, InputType, Vector(Input) /= error).

prop_check_dot(Vector) ->
    %generate input type
    InputType = union([{dot, [1], [1]}]),
    ?FORALL(Input, InputType, Vector(Input) /= error).

prop_check_mul(Vector) ->
    %generate input type
    InputType = union([{mul, 1, [1]}]),
    ?FORALL(Input, InputType, Vector(Input) /= error).

prop_check_div(Vector) ->
    %generate input type
    InputType = union([{'div', 1, [1]}]),
    ?FORALL(Input, InputType, Vector(Input) /= error).

prop_check_norm_one(Vector) ->
    %generate input type
    InputType = union([{mul, {norm_one, [0,1,2,3,4,5]}, [1]}]),
    ?FORALL(Input, InputType, Vector(Input) /= error).

prop_check_norm_inf(Vector) ->
    %generate input type
    InputType = union([{mul, {norm_inf, [0,1,2,3,4,5]}, [1]}]),
    ?FORALL(Input, InputType, Vector(Input) /= error).

% CHECK THAT DIVISION BY 0 FAILS
prop_check_zero_div(Vector) ->
    %generate input type
    InputType = union([{'div', 0, [1]}]),
    ?FORALL(Input, InputType, Vector(Input) =:= error).

%EMPTY OR LONG VECTORS
prop_check_empty_vector(Vector) ->
    ?FORALL(Input, gen_vector(0), Vector(Input) =:= error).

prop_check_long_vector(Vector) ->
    ?FORALL(Input, gen_long_vector(), Vector(Input) =:= error).

prop_check_vector_eval(Vector) ->
    ?FORALL(Input, gen_random_valid_vector(), Vector(Input) =:= eval_expr(Input)).

prop_check_single_element(Vector) ->
    ?FORALL(Input, gen_vector(1), Vector(Input) =:= eval_expr(Input)).

%DIFFERENT ARGUMENT LENGTH ON VECTOR OPERATIONS
prop_check_arg_length(Vector) ->
    ?FORALL(Input, gen_dif_length_vector_op(), Vector(Input) =:= error).

% CHECK NESTING SUPPORT FOR THE RECERSIVE 
prop_check_nesting_support(Vector) ->
    InputType = union([
        {add, {add, [1], [1]}, {add, [1], [1]}},
        {sub, {sub, [1], [1]}, {sub, [1], [1]}},
        {dot, {dot, [1], [1]}, {dot, [1], [1]}},
        {mul, 1, {mul, 1, [1]}},
        {'div', 1, {'div', 1, [1]}}
        ]),
    ?FORALL(Input, InputType, Vector(Input) /= error).

%CHECK EXACTLY 100 TIMES NESTED EXPRESSIONS
prop_100nested_vector_op(Vector) ->
    ?FORALL(Input, borderline_vector_op_expr(), Vector(Input) /= error).

prop_100nested_scalar_op(Vector) ->
    ?FORALL(Input, borderline_scalar_op_expr(), Vector(Input) /= error).


%MORE THAN 100 NESTING DEPTH EXPRESSIONS
prop_deep_vector_op(Vector) ->
    ?FORALL(Input, deep_vector_op_expr(), Vector(Input) =:= error).

prop_deep_scalar_op(Vector) ->
    ?FORALL(Input, deep_scalar_op_expr(), Vector(Input) =:= error).

% CHECK OUTPUT FOR VALID INPUTS ONLY. MOSTLY FOR ARITHMETIC EVALUATION
prop_general_arithmetic_comprarison(Vector) ->
    ?FORALL(Input, valid_input_expr(), Vector(Input) =:= eval_expr(Input)).


%===========================================================
%Some Input/Operation Generators needed for the tests
%===========================================================

% Simple generates to use for building bigger
% and more complex inputs
gen_vector_op() -> union([add, sub, dot]).
gen_scalar_op() -> union([mul, 'div']).
gen_norm_op() -> union([norm_one, norm_inf]).

% (Need a generator for specific length vectors to
% ensure vector ops get arguments of the same dimension)
gen_vector(L) ->
    union([vector(L, integer())]).

gen_long_vector() ->
    ?LET(L, range(101,110), gen_vector(L)).

gen_random_valid_vector() ->
    ?LET(L, range(1,100), gen_vector(L)).

gen_dif_length_vector_op() ->
    union([{gen_vector_op(), gen_vector(2), gen_vector(3)}]).

%Generates more than 100 times nested vector operation expr
%some are off by one like vector 47 as it was found from trial and error
%and the property does not fail on 101 times nested expressions
deep_vector_op_expr() ->
    ?LET(N, range(101,105), deep_vector_op_expr(N)).
deep_vector_op_expr(1) -> gen_vector(2);
deep_vector_op_expr(N) -> 
    union([{gen_vector_op(), gen_vector(2), deep_vector_op_expr(N-1)}]).

deep_scalar_op_expr() ->
    ?LET(N, range(101,105), deep_scalar_op_expr(N)).
deep_scalar_op_expr(1) -> gen_vector(2);
deep_scalar_op_expr(N) ->
    union([{gen_scalar_op(), 1, deep_scalar_op_expr(N-1)}]).

%Generate exactly 100 times nested operations
%(considering that 100 is the critical conditions
%of failure for nested operations, checking both 100 and 101)
borderline_vector_op_expr() ->
    ?LET(N, union([100]), deep_vector_op_expr(N)).

borderline_scalar_op_expr() ->
    ?LET(N, union([100]), deep_scalar_op_expr(N)).

% Generator for more complex and  valid expressions
% Use smaller range instead of random 1-100  vector length
% so testing won't take forever
% Also decrease to 5 nesting levels due to exponential
% growth of the input
valid_input_expr() ->
    ?LET(N, range(1,7), gen_vector_expr(N)).
gen_vector_expr(N) ->
    V = rand:uniform(10),
    gen_vector_expr(N, V).
gen_vector_expr(0, V) -> gen_vector(V);
gen_vector_expr(N, V) ->
    union([
        gen_vector(V),
        {gen_vector_op(), gen_vector_expr(N-1, V), gen_vector_expr(N-1, V)},
        {gen_scalar_op(), gen_int_expr(N-1), gen_vector_expr(N-1, V)}
    ]).

gen_int_expr(0) -> pos_integer();
gen_int_expr(N) ->
    V = rand:uniform(10),
    union([
        pos_integer(),
        {gen_norm_op(), gen_vector_expr(N-1, V)}
    ]).



%===========================================================
% My own evaluator implimentation
%===========================================================

%evaluate expression
-spec eval_expr(expr()) -> eval_result().
eval_expr(Expr) -> eval_expr(Expr, 1).
%evaluate vector operations
-spec eval_expr(expr(), pos_integer()) -> eval_result().
eval_expr(_, Depth) when Depth > 100 -> error;
eval_expr(V ,_) when is_list(V) -> eval_vector(V);
eval_expr({Op, E1, E2}, Depth) when Op =:= add orelse Op =:= sub orelse Op =:= dot ->
    V1 = eval_expr(E1, Depth+1),
    V2 = eval_expr(E2, Depth+1),
    if
        V1 =:= error orelse V2 =:= error orelse length(V1) /= length(V2) ->
            error;
        true ->
            case Op of
                add -> lists:zipwith(fun(X, Y) -> X + Y end ,V1, V2);
                sub -> lists:zipwith(fun(X, Y) -> X - Y end ,V1, V2);
                dot -> lists:zipwith(fun(X, Y) -> X * Y end ,V1, V2);
                _ -> error
            end
    end;
eval_expr({Op, E1, E2}, Depth) when Op =:= mul orelse Op =:= 'div' ->
    V1 = eval_int_expr(E1, Depth+1),
    V2 = eval_expr(E2, Depth+1),
    if
        V1 =:= error orelse V2 =:= error ->
            error;
        true ->
            case Op of
                mul -> lists:map(fun(X) -> X * V1 end, V2);
                'div' ->
                    if
                        V1 /= 0 -> lists:map(fun(X) -> X div V1 end, V2);
                        true -> error
                    end;
                _ -> error
            end
    end;
eval_expr(_, _) -> error.

%evaluate integer expression
-spec eval_int_expr(int_expr() ,pos_integer()) -> integer().
eval_int_expr(I, _) when is_integer(I) -> I;
eval_int_expr({Norm, Expr}, Depth) when Depth < 101 ->
    V = eval_expr(Expr, Depth+1),
    if
        V =:= error ->
            error;
        true ->
            case Norm of
                norm_one -> lists:foldl(fun(X,Y) -> abs(X)+Y end, 0, V);
                norm_inf -> lists:foldl(fun(X,Y) -> max(abs(X),Y) end, 0, V);
                _ -> error
            end
    end;
eval_int_expr(_, _) -> error.

%evaluate/check vector dimension (between 1 and 100) and elements' type (integers)
-spec eval_vector(vector()) -> eval_result().
eval_vector(V) when V /= [] andalso length(V) < 101 -> check_elements(V, []);
eval_vector(_) -> error.
check_elements([X], Acc) when is_integer(X) -> lists:reverse([X|Acc]);
check_elements([X|XS], Acc) when is_integer(X) -> check_elements(XS, [X|Acc]);
check_elements(_, _) -> error.

%just some eunit tests to check my evaluator
demo_eval_test_() ->
    [
        ?_assertEqual([1,2,3,4,5], eval_expr([1,2,3,4,5])),
        ?_assertEqual([42,42,42], eval_expr({'dot', [6,6,6], [7,7,7]})),
        ?_assertEqual([42,-42,42], eval_expr({'mul', {'norm_one', [1,-1,2,-2]}, [7,-7,7]})),
        ?_assertEqual(error, eval_expr({'div', 0, [1,2,3,4,5]})),
        ?_assertEqual(error, eval_expr(lists:seq(1,101)))
    ].