:- module(jugs_tests, []).
:- use_module(jugs).
:- use_module(library(plunit)).

:- begin_tests(jugs).
test(minimum_moves) :-
    solve_jugs_shortest(Path),
    length(Path, 6).
:- end_tests(jugs).

:- initialization(run_tests, main).

