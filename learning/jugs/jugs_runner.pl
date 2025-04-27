:- use_module(jugs).

:- initialization(run).

run :-
    % Can also call this as:
    % syrup:solve_jugs(Path),
    solve_jugs(Path),
    writeln('Solution Path:'),
    writeln(Path),
    halt.
