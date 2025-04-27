% Necessary when working with constraints and arithmetic using #=/2 (the CLP(FD) constraint operator)
% rather than =/2 (the traditional arithmetic operator).
:- use_module(library(clpfd)).

% Interpret "abc" as [97, 98, 99] instead of atom or string.
:- set_prolog_flag(double_quotes, codes).

% The lenggth of the empty list [] is 0.
list_length([], 0).

% If the length of the list Ls is N0 and N is N0 + 1, then the length of [_|Ls] is N.
% Furthermore, this only holds if N is greater than 0.
list_length([_|Ls], N) :-
    N #> 0,
    N #= N0 + 1,
    list_length(Ls, N0).

% Without constraint logic programming (CLP), the above code would be:
list_length2([], 0).
list_length2([_|Ls], N) :-
    list_length2(Ls, N0),
    N is N0 + 1.
% The above code is not as efficient as the CLP version because it uses the is/2 operator, which is not as efficient as the #= operator.
% The is/2 operator is used to evaluate arithmetic expressions, while the #= operator is used to define constraints.

% To reload a file in SWI-Prolog, use the following command:
% ?- [lists].
% or:
% ?- consult(lists).

