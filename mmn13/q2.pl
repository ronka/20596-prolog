
assign_numbers(X) :-
    assign_numbers(X,1,_).
    
assign_numbers(X,Count,Count1) :-
    % if X is variable
    var(X),
    X = Count,
    Count1 is Count + 1,!
    ;
    % if X is list
    X = [H|T],
    concat(H,T,Temp),
    assign_numbers(Temp,Count,Count1),!
    ;
    % if X is a paradict
    compound(X),
    X =.. [_|Args],
    assign_numbers(Args,Count,Count1),!
    ;
    % if X is none above, it's an atom, skip
    Count1 is Count.
    
assign_numbers([],_,_).