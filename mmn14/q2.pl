
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

build_tree(T):-
    T=t(t(t(t(nil,6,t(nil,7,nil)),3,nil),2,t(nil,4,t(nil,5,nil))),1,t(t(t(nil,10,nil),9,t(t(nil,0,nil),11,nil)),8,nil)).
