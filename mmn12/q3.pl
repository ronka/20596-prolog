
solution(X11,X12,X13,X21,X22,X23,X31,X32,X33):-
    % get all possible combos of numbers 1 to 9
    permutation([X11,X12,X13,X21,X22,X23,X31,X32,X33],[1,2,3,4,5,6,7,8,9]),

    % calc rows
    44 is X11 * X12 + X13,
    9 is X21 - X22 + X23,
    2 is X31 * X32 / X33,

    % calc columns    
    15 is X11 + X21 * X31,
    8 is X12 + X22 * X32,
    66 is X13 * X23 - X33.

permutation([X|Xs],Y):-
    del(X,Y,Rest),
    permutation(Xs,Rest).
permutation([],[]).

del(X,[X|Z],Z).
del(X,[Y|Z],[Y|Z1]):-
	del(X,Z,Z1).