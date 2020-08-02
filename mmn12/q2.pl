

long_mult(Num1, Num2, Res):-
    long_mult(Num1, Num2, [0], Res).

% the idea is to multiply one number at a time
long_mult(Num1, [Num2|T2], [Rest|Trest], [Res|Tres]):-
    % extract least significant number from multiplication
    list_mult(Num1, Num2, [LongMultHead|LongMultTail]),
    % add the rest to that number, save the number
    Res is (LongMultHead + Rest) mod 10,
    % caculate the rest to pass on, add to it the rest from previous call
    Res1Rest is (LongMultHead + Rest) // 10,
    long_add(Trest,[Res1Rest],Trest1),
    long_add(Trest1, LongMultTail, LongAddRes),
    % go to next number
    long_mult(Num1, T2, LongAddRes, Tres).

long_mult( _, [], Z, Z ).


%%%%%%%%%%%
% Helpers %
%%%%%%%%%%%

% Long add for numbers represnted by lists with least significant number first %

long_add( Num1, Num2, Res ):-
    long_add( Num1, Num2, 0, Res ).

long_add( [Num1|T1], [Num2|T2], Rest, [Res|Tres] ):-
    Res is ( Num1 + Num2 + Rest ) mod 10, % get the result of sum without the rest (13 => 3)
    Rest1 is ( Num1 + Num2 + Rest ) // 10, % get the rest (13 => 1)
    long_add( T1, T2, Rest1, Tres ).

% if number2 is longer than number1
long_add( [], [Num2|T2], Rest, [Res|Tres] ):-
    Res is ( Num2 + Rest ) mod 10, % get the result of sum without the rest (13 => 3)
    Rest1 is ( Num2 + Rest ) // 10, % get the rest (13 => 1)
    long_add( [], T2, Rest1, Tres ).

% if number1 is longer than number2
long_add( [Num1|T1], [], Rest, [Res|Tres] ):-
    Res is ( Num1 + Rest ) mod 10, % get the result of sum without the rest (13 => 3)
    Rest1 is ( Num1 + Rest ) // 10, % get the rest (13 => 1)
    long_add( T1, [], Rest1, Tres ).

long_add( [], [], 0, [] ).
long_add( [], [], 1, [1] ).


% one number multiply a list

list_mult( Num1, X, Res ):-
    list_mult( Num1, X, 0, Res ).

list_mult( [Num1|T1], X, Rest, [Res|Tres] ):-
    Res is (Num1 * X + Rest) mod 10, % get the result of multi without the rest (13 => 3)
    Rest1 is (Num1 * X + Rest) // 10, % get the rest (13 => 1)
    list_mult( T1, X, Rest1, Tres ).
list_mult( [], _, 0, [] ).
list_mult( [], _, Z, [Z] ):-
    Z \= 0.