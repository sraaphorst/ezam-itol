:- module(jugs, [solve_jugs/1,
                 solve_jugs/2,
                 solve_jugs_shortest/1]).

:- use_module(library(clpfd)).

% Stop when we find a jug with 4 liters of syrup.
moves(Jugs) --> { member(jug(_, _, 4), Jugs) }.

moves(Jugs) --> [from_to(From, To, Amount)],
    { select(jug(From, FromCapacity, FromQuantity), Jugs, Jugs1),
      select(jug(To, ToCapacity, ToQuantity), Jugs1, JugsRest),
      FromQuantity #> 0,
      ToQuantity #< ToCapacity,
      Amount #= min(FromQuantity, ToCapacity - ToQuantity),
      FromQuantityNew #= FromQuantity - Amount,
      ToQuantityNew #= ToQuantity + Amount },
      moves([jug(From, FromCapacity, FromQuantityNew),
             jug(To, ToCapacity, ToQuantityNew)
             | JugsRest]).

% We use iterative deepening to find a solution with a given depth.
solve_jugs(Path, Depth) :-
    Initial = [jug(a, 5, 0), jug(b, 3, 0), jug(c, 8, 8)],
    length(Path, Depth),
    phrase(moves(Initial), Path).

% To solve by restricting the depth of the search, we can use the following code.
% It will solve from the shortest path onwards.
solve_jugs(Path) :-
    Initial = [jug(a, 5, 0), jug(b, 3, 0), jug(c, 8, 8)],
    length(Path, _),
    phrase(moves(Initial), Path).

% Find the solution with the minimum number of moves.
solve_jugs_shortest(Path) :-
    between(0, inf, Depth),
    format('Trying ~w moves...~n', [Depth]),
    solve_jugs(Path, Depth),
    !.
