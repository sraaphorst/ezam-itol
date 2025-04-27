:- module(hanoi_tests, []).
:- use_module(hanoi).
:- use_module(library(plunit)).

:- begin_tests(hanoi).

% -- constructive -----------------------------------------------------

test(size3_construct) :-
    hanoi_construct(3, Moves),
    length(Moves, 7).

test(size4_construct) :-
    hanoi_construct(4, Moves),
    length(Moves, 15).

% -- search-based -----------------------------------------------------

test(size3_search) :-
    hanoi_search_shortest(3, Path),
    length(Path, 7).

test(size4_search) :-
    hanoi_search_shortest(4, Path),
    length(Path, 15).

:- end_tests(hanoi).

% Execute the tests when the file is loaded.
% swipl hanoi_tests.pl
:- initialization(run_tests, main).