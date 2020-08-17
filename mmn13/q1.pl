
% a - with cut
% delete_one(X,[X|Xs],Xs):-!.
% delete_one(X,[Y|Ys],[Y|Xs]):-
%     delete_one(X,Ys,Xs).


% a - with not
delete_one(X,[X|Xs],Xs).
delete_one(X,[Y|Ys],[Y|Xs]):-
    delete_one(X,Ys,Xs),
    not(X = Y).


% b
delete_all(_,[],[]).
delete_all(X,[X|Xs],Ys):-
    % once found what to delete, cut and continue to delete
    !,delete_all(X,Xs,Ys).
delete_all(X,[Y|Ys],[Y|Xs]):-
    delete_all(X,Ys,Xs).