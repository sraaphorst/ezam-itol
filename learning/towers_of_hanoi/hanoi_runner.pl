% This, or $ swipl --stack-limit=8G hanoi_runner.pl -- 25
% :- set_prolog_flag(stack_limit, 8_589_934_592).

:- use_module(hanoi).

:- initialization(run).

run :-
    current_prolog_flag(argv, Argv),
    (
        Argv = [NAtom] ->
            atom_number(NAtom, N),
        execute(N)
    
    ;  Argv = [] ->
        % Default to 3 disks if no number is provided.
        writeln('No number of disks provided.'),
        writeln('Defaulting to 3 disks.'),
        execute(3)
        
    ;   writeln('Usage: swipl hanoi_runner.pl -- <number_of_disks>'),
        writeln('Default is 3 disks.'),
        halt(1)
    ),
    halt.

execute(N) :-
    % Begin calculating statistics.
    statistics(runtime, [StartTime | _]),

    % Solve the problem and pretty-print the solution.

    % Perform using search.
    % hanoi_search_shortest(N, Path),

    % Perform using construction.
    hanoi_construct(N, Path),

    length(Path, Moves),
    format('Solution Path: ~w moves.~n', [Moves]),
    pretty_print_moves(Path),

    % Check that the expected number of moves was achieved.
    ExpectedMoves is 2**N - 1,
    (
        Moves =:= ExpectedMoves ->
            writeln('Correct number of moves achieved.')
        ; format('Incorrect number of moves: expected ~w, got ~w.~n', [ExpectedMoves, Moves])
    ),

    % End calculating statistics.
    statistics(runtime, [EndTime | _]),
    ElapsedTime is EndTime - StartTime,
    (
        ElapsedTime < 5000 ->
            format('Elapsed runtime: ~w ms.~n', [ElapsedTime])
        ; ElapsedTimeInSeconds is ElapsedTime / 1000,
          format('Elapsed runtime: ~2f seconds.~n', [ElapsedTimeInSeconds])
    ),

    % Alert if the solving took too long.
    (ElapsedTime > 12000 ->
        writeln('⚠️  Warning: Solving took unusually long!')
     ; true
    ).
