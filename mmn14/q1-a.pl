
mis(nil,[],0):- !.
mis(Tree,List,N):-
	misPlus(Tree,PList,PlusN),
    misMinus(Tree,MList,MinusN),
	(
        MinusN >= PlusN,
        List = MList,
        N = MinusN
    ;
        PlusN >= MinusN,
        List = PList,
        N = PlusN 
	).

misPlus(nil,[],0):- !. 
misPlus(t(L,X,R),[X|List],N):-
	misMinus(L,LList,LN),
	misMinus(R,RList,RN),
	conc(LList,RList,List),
	N is LN + RN + 1.

misMinus(nil,[],0):- !.
misMinus(t(L,_,R),List,N):-
	mis(L,LList,LN),
	mis(R,RList,RN),
	conc(LList,RList,List),
	N is LN + RN.


%%%%%%%% helpers %%%%%%%%

build_tree(T):-
    T=t(t(t(t(nil,6,t(nil,7,nil)),3,nil),2,t(nil,4,t(nil,5,nil))),1,t(t(t(nil,10,nil),9,t(t(nil,0,nil),11,nil)),8,nil)).

conc([X|Xs],L,[X|Ys]):-
	conc(Xs,L,Ys).
conc([],L,L).