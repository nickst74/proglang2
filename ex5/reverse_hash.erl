% Σταματελόπουλος Νικόλαος
% Α.Μ. : 03116138

-module(reverse_hash).
%-compile(export_all).

-export([solve/4]).

% Search space is from 1 to (2^27)-1 = 134217727
-define (UPPER_BOUND, 134217727).%(math:pow(2,27)-1)).

-type hash_function() :: fun((integer()) -> integer()).
-type hash_pair() :: {integer(), integer()}.

% Master process spawns the slaves that make the computations
% Then stops them receives the results and send them to the
% Parent process (the grader)
-spec solve(hash_function(), [integer()], integer(), integer()) -> ok.
solve(HashFun, Inputs, Par, Schedulers) ->
    Master = self(),
    % Spawn slave processes equal to Schedulers - 1
    % Each slave gets a part of the search space
    CutSize = ?UPPER_BOUND div Schedulers,
    Slaves = [spawn_link(fun() -> Master ! slave(HashFun, Inputs, N*CutSize+1) end) || N <- lists:seq(1, Schedulers - 1)],
    % run the last slave on master process
    % (so processes are equal to the schedulers that we have)
    PartialResult = slave(HashFun, Inputs, 1),
    % Once time is up shutdown slaves and merge the partial results
    shutdown_slaves(Slaves),
    Ret = collect_results(Slaves ,[PartialResult]),
    Par ! {reply, Ret},
    ok.

% Sends shutdown signal to slaves
-spec shutdown_slaves([integer()]) -> ok.
shutdown_slaves([]) -> ok;
shutdown_slaves([X|XS]) ->
    X ! finish_up,
    shutdown_slaves(XS).

% Collect results from mailbox
-spec collect_results([integer()], [[hash_pair()]]) -> [hash_pair()].
collect_results([], Acc) -> lists:flatten(Acc);
collect_results([_|XS], Acc) ->
    receive
        PartialResult -> collect_results(XS, [PartialResult|Acc])
    end.

% Slaves are responsible for all computations on the Hash Function
-spec slave(hash_function(), [integer()], integer()) -> [hash_pair()].
slave(HashFun, Inputs, StartingIteration) ->
    % Using process dictionary as passing a set through every worker()
    % iteration crippled the performance
    store_inputs(Inputs),
    worker(HashFun, StartingIteration, []).

% Store inputs to the process dictionary
-spec store_inputs([integer()]) -> ok.
store_inputs([]) -> ok;
store_inputs([X|XS]) ->
    put(X, true),
    store_inputs(XS).

% Brute force the HashFunction while checking
% if time is up in every iteration
% (we could set and interval to check the mailbox every N iteration
% but did not seem to add quit an overhead)
-spec worker(hash_function(), integer(), [hash_pair()]) -> [hash_pair()].
worker(HashFun, It, Ret) ->
    receive
        finish_up -> Ret
    after
        0 ->
            Hash = HashFun(It),
            case get(Hash) of
                true -> worker(HashFun, It+1, [{Hash, It}|Ret]);
                undefined -> worker(HashFun, It+1, Ret)
            end
    end.