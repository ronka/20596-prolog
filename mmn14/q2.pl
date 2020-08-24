
% override
maxSubTree(Tree,Res):-
    maxSubTree(Tree,_,_,Res).


/**
 * 
 * t(L,X,R) - the tree to analyze
 * Sum - total sum of tree
 * ResSum - sum of the result tree
 * Res - result tree
 * 
 */
maxSubTree(t(L,X,R),Sum,ResSum,Res):-
    !,
    maxSubTree(L,SL,LResSum,LRes),
    maxSubTree(R,SR,RResSum,RRes),
    Sum is SL + SR + X,
    (
        RRes \= nil, % If right tree exists, 
        RResSum >= LResSum, % and his sum is bigger than left tree's sum 
        RResSum >= Sum, % and bigger than total sum of tree
        
        Res = RRes, % save the tree for result
        ResSum = RResSum,! % and save the tree's sum
	;
        LRes \= nil,
        LResSum >= RResSum,
        LResSum >= Sum ,
        
        Res = LRes,
        ResSum = LResSum,!
    ;
        % if none the above it means this tree has the biggest sum, save it
        Sum = ResSum,
        Res = t(L,X,R)
	).

maxSubTree(nil,0,0,nil).

/*
path_to_item(X,t(,X,),[X]).

path_to_item(X,t(L,Y,R),[Y|Path]):-
            path_to_item(X,L,Path)==
			;
            path_to_item(X,R,Path).


%T=t(t(t(t(nil,22,nil),5,nil),24,nil),4,t(t(nil,5,nil),6,t(nil,8,nil))),path_to_item(22,T,Res)

T=t(t(t(nil,-5,nil),4,t(t(nil,15,nil),-20,t(nil,10,nil))),2,t(nil,-8,t(t(nil,9,nil),12,t(nil,10,nil)))), maxSubTree(T,Res).
T=t(nil,21,t(nil,3,nil)),maxSubTree(T,Res).

height(nil,-1).
height(t(L,_,R),H):-
    height(L,HL),
    height(R,HR),
    (HL>HR,!,H is HL+1
    ;   
    H is HR + 1).

sum(nil,0).
sum(t(L,X,R),Res):-
   sum(L,ResL),
    sum(R,ResR),
    Res is ResL+ResR+X.
    
    
sumH(nil,0).
sumH(t(L,X,R),Res):-
    sumH(L,ResL),
    sumH(R,ResR),
    height(t(L,X,R),H),
    Res is ResL+ResR+H.

sumH(T,Res,_).   
sumH(L,0,-1).
sumH(t(L,X,R),Res,H):-
    sumH(L,ResL,HL),
    sumH(R,ResR,HR),
    max(HL,HR,TH),H is TH +1,
    Res is ResL+ResR+H.

*/