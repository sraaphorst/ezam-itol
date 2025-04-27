% The solution to the jugs problem is to find a sequence of moves that will fill a jug
% with 2 liters of water.
:- use_module(library(clpfd)).

% This is the solution using DCGs (Definite Clause Grammars), which are like monads in other languages.
% The DCG is a way to represent a sequence of elements in a list, and it can be used to represent a sequence of actions.
moves(Js0) --> { member(jug(_,_,2), Js0) }.

moves(Js0) --> [from_to(F,T)],
    { select(jug(F,FC,FF0), Js0, Js1),
      select(jug(T,TC,TF0), Js1, Js),
      % Commenting out this line will make the code work in terms of finding the shortest path, but
      % in the entire set of paths, it will allow jugs to have negative fill levels.
      % FF0 #> 0, TF0 #< TC, M #= min(FF0, TC-TF0),
      M #= min(FF0, TC-TF0),
      FF #= FF0 - M, TF #= TF0 + M },
      moves([jug(F,FC,FF), jug(T,TC,TF)|Js]).

% We use iterative deepening to find a solution with a given depth.
% Indeed, iterative deepening finds the shortest path to a solution, which might not happen if we stored
% the history of the path.
% Solves for Depth 4, i.e. solve_jugs(Path, 4) gives:
% Path = [from_to(c, b), from_to(b, a), from_to(c, b), from_to(b, a)]
solve_jugs(Path, Depth) :-
    Initial = [jug(a,4,0), jug(b,3,0), jug(c,7,7)],
    length(Path, Depth), phrase(moves(Initial), Path).


% To solve by restricting the depth of the search, we can use the following code.
% It will solve from the shortest path onwards.
solve_jugs(Path) :-
    Initial = [jug(a,4,0), jug(b,3,0), jug(c,7,7)],
    length(Path, _),
    phrase(moves(Initial), Path).

