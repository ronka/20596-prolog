

% mis1 overloads mis in order to get rid of the tail in the diff-list
mis(Tree,List,N):-
    mis1(Tree,List-[],N).


mis1(nil,Tail-Tail,0):- !.
mis1(Tree,List,N):- 
    misPlus(Tree,PList,PN),
    misMinus(Tree,MList,MN),
    (
        MN >= PN,
        List = MList,
        N = MN
    ;
        PN >= MN,
        List = PList,
        N = PN 
    ).


misPlus(nil,Tail-Tail,0):- !. 
misPlus(t(L,X,R),[X|List]-Tail,N):-
    misMinus(L,LList,LN),
    misMinus(R,RList,RN),
    conc(LList,RList,List-Tail),
    N is LN + RN + 1.

misMinus(nil,Tail-Tail,0):- !.
misMinus(t(L,_,R),List,N):-
    mis1(L,LList,LN),
    mis1(R,RList,RN),
    conc(LList,RList,List),
    N is LN + RN.


%%%%%%%% helpers %%%%%%%%

build_tree(T):-
    T=t(t(t(t(nil,6,t(nil,7,nil)),3,nil),2,t(nil,4,t(nil,5,nil))),1,t(t(t(nil,10,nil),9,t(t(nil,0,nil),11,nil)),8,nil)).

conc(List1-List2,List2-Tail2,List1-Tail2).