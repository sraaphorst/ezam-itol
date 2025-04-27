:- module(hanoi, [hanoi_search/1,
                  hanoi_search/2,
                  hanoi_search/3,
                  hanoi_search_shortest/1,
                  hanoi_search_shortest/2,
                  hanoi_construct/1,
                  hanoi_construct/2,
                  pretty_print_moves/1]).

% No longer necessary to use the clpfd library for this code.
% :- use_module(library(clpfd)).

% The solution to the Towers of Hanoi problem is to find a sequence of moves that will move
% all the disks from one peg to another peg, using a third peg as an auxiliary peg.

% We have solved the problem when peg c has all the disks in sorted order.
solved(N, Pegs) :-
    member(peg(c, Disks), Pegs),
    length(Disks, N),
    sort(Disks, Disks).

moves(N, Pegs) --> {
    solved(N, Pegs)
 }.

% We can avoid using the select/3 predicate by using the DCG notation.
% Can always move a top disk to an empty peg.
moves(N, Pegs) --> [from_to(From, To, Disk)],
    { select(peg(From, [Disk | FromRestDisks]), Pegs, Pegs1),
      select(peg(To, []), Pegs1, PegLeft)
    },
    moves(N, [peg(From, FromRestDisks), peg(To, [Disk]) | PegLeft]).

% Can move a top disk from one peg to another peg if the destination peg has a bigger disk on top.
moves(N, Pegs) --> [from_to(From, To, Disk)],
    { select(peg(From, [Disk | FromRestDisks]), Pegs, Pegs1),
      select(peg(To, [TopDisk | ToRestDisks]), Pegs1, PegLeft),
      Disk < TopDisk },
    moves(N, [peg(From, FromRestDisks), peg(To, [Disk, TopDisk | ToRestDisks]) | PegLeft]).

% We use iterative deepening to find a solution with a given depth.
% Could also use the following to generate the list of disks:
% findall(Disk, between(1, N, Disk), Disks).
hanoi_search(N, Path, Depth) :-
    Initial = [peg(a, Disks), peg(b, []), peg(c, [])],
    numlist(1, N, Disks),
    length(Path, Depth),
    phrase(moves(N, Initial), Path).

hanoi_search(N, Path) :-
    Initial = [peg(a, Disks), peg(b, []), peg(c, [])],
    numlist(1, N, Disks),
    length(Path, _),
    phrase(moves(N, Initial), Path).

hanoi_search(Path) :-
    hanoi_search(3, Path).

% Find the solution with the minimum number of moves.
hanoi_search_shortest(N, Path) :-
    between(0, inf, Depth),
    format('Trying depth ~w...~n', [Depth]),
    hanoi_search(N, Path, Depth),
    !.

% Default to 3 disks if no number is provided.
hanoi_search_shortest(Path) :-
    hanoi_search_shortest(3, Path).

% Instead of searching for the shortest path, we can use the following code to directly construct
% the solution.
hanoi_recursive(0, _, _, _, []) :- !.

hanoi_recursive(N, From, To, Aux, Moves) :-
    N > 0,
    N1 is N - 1,
    hanoi_recursive(N1, From, Aux, To, Moves1),
    Moves2 = [from_to(From, To, N) | MovesMid],
    hanoi_recursive(N1, Aux, To, From, MovesMid),
    append(Moves1, Moves2, Moves).

% We give c as the second parameter as that is the destination peg (To).
hanoi_construct(N, Moves) :-
    hanoi_recursive(N, a, c, b, Moves).

hanoi_construct(Moves) :-
    hanoi_construct(3, Moves).

% Pretty-printing the solution.
pretty_print_moves([]).

pretty_print_moves([from_to(From, To, Disk) | Moves]) :-
    upcase_atom(From, UpperFrom),
    upcase_atom(To, UpperTo),
    format('Move disk ~w from peg ~w to peg ~w.~n', [Disk, UpperFrom, UpperTo]),
    pretty_print_moves(Moves).
