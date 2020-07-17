in_queue(adi, 1, null).
in_queue(rami, 1, adi).
in_queue(yael, 1, rami).
in_queue(yuval, 1, yael).
in_queue(dana, 2 , null).
in_queue(tzvi, 2, dana).
in_queue(alon, 2, tzvi).
in_queue(vered, 2, alon).
in_queue(david, 2, vered).

/************ A ************/
touch(Name1,Name2):-
    in_queue(Name1, _, Name2)
    ;
    in_queue(Name2, _, Name1).

/************ B ************/
interior(Name):-
    in_queue(Name,_,X), in_queue(Y,_,Name), X\=null, X\=Y.

/************ C ************/
before(Name1,Name2):- in_queue(Name2, _, Name1).
before(Name1,Name2):- 
    in_queue(X, _, Name1),
    before(X,Name2).

/************ D ************/
% stop conditions
earlier(Name1,Name2):-
    % if in the same queue
    before(Name1,Name2)
    ;
    % else if different queues
    in_queue(Name1,_,null), in_queue(Name2,_,X), X\=null.
earlier(Name1,Name2):- 
    % else if advance for next in queue and check again
    in_queue(Name1,_,X), in_queue(Name2,_,Y),
    earlier(X,Y).

/************ E ************/
% stop condition
same_time_12(Name1,Name2):- 
    in_queue(Name1,1,null), in_queue(Name2,2,null).
same_time_12(Name1,Name2):- 
    % advance queue 1 by 1 and queue 2 by 2
    in_queue(Name1,1,X),
    in_queue(Name2,2,Y), in_queue(Y,2,Z),
    same_time_12(X,Z).