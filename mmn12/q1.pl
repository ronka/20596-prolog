
% stop condition
long_conc([],[]).

% if inside set is empty
long_conc([[]|Tail],Res):-
    long_conc(Tail,Res).

% get the head and append to new set
long_conc([[X|Xs]|Tail],[X|Res]):-
    long_conc([Xs|Tail],Res).