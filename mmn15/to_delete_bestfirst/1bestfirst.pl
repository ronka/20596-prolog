:- dynamic square/2.
:- dynamic downOrRightNeighbor/3.
:- dynamic onPath/1.
:- dynamic origin/1.
:- dynamic goal/1.

%main and misc. :
%solve the maze in the file named InFileName and print the solution to OutFileName
solve(InFileName,OutFileName):-
	cleanDynamics,
	readInput(InFileName),
	engineSolve(Path),
	markPath(Path),
	writePath(InFileName,OutFileName).

%delete all facts created during previous program runs.
cleanDynamics:-
	retractAll( square(_,_) ),
	retractAll( downOrRightNeighbor(_,_,_) ),
	retractAll( onPath(_) ),
	retractAll( origin(_) ),
	retractAll( goal(_) ).
%S is the target(end) of the maze
target(S):- goal(S).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Input:
/*dynamics:
square(X,Y)
downOrRightNeighbor(S1,S2,Aprroved).
*/ 
%true iff there is a passage between the squares S1 and S2.
passage(S1,S2):- downOrRightNeighbor(S1,S2,yes);downOrRightNeighbor(S2,S1,yes).

%transforms the input in InFileName to the relevant facts for solving the maze.
readInput(InFileName):-
	see(InFileName),
	doOnce(readInputHelp(1)),
	retractUnaprroved, 
	close(InFileName).
%removes all the downOrRightNeighbor facts which in it there is a space in the separating line but there is no square under that space .
retractUnaprroved:-retractAll(downOrRightNeighbor(S1,S2,no)).

%Help function for readInput
readInputHelp(Y):-
	Y1 is Y+1,
	Y2 is Y+2,
	doOnce(readSquareRow(0,Y)),
	getChar(_), %skip \n char
	doOnce(readSeparatorRow(0,Y1)),
	getChar(_), %skip \n char
	readInputHelp(Y2).
%read a row of squares(|/t/s/space characters) starting from the location (X,Y). 
readSquareRow(X,Y):-
	getChar(Char),
	not (X=0,Char = ' '),
	Char \= '~M',
	handleSquareRowChar(Char,X,Y),
	X1 is X+1,
	readSquareRow(X1,Y).

%read a separtaing row(space/- characters) starting from the location (X,Y).
readSeparatorRow(X,Y):-
	getChar(Char),
	Char \= '~M' ,
	handleSepRowChar(Char,X,Y),
	X1 is X+1,
	readSeparatorRow(X1,Y).

%decide what to do with the character Char located in (X,Y) and commit it.
handleSquareRowChar(Char,X,Y):-
	CurrSquare=square(X,Y),
	(
	Char = 's',
	assert(origin(CurrSquare)),
	addSquare(CurrSquare)
	;
	Char = 't',
	assert(goal(CurrSquare)),
	addSquare(CurrSquare)
	;
	Char = ' ',
	addSquare(CurrSquare)
	;
	Char ='|'
	).
%add CurrSquare to the square DB and approve that it can be a neighbor of the square above it(if exsits),and on the left of it
addSquare(CurrSquare):-
	CurrSquare = square(X,Y),
	assert(CurrSquare),
	doOnce(approveNeighbor(CurrSquare)),
	X1 is X-1,
	(square(X1,Y),assert(downOrRightNeighbor(square(X1,Y),CurrSquare,yes)),!;true ).

%approve that square(X,Y)can be a neighbor of the square above it(if exsits)
approveNeighbor(square(X,Y)):-
	YM2 is Y-2,
	Unapproved=downOrRightNeighbor(square(X,YM2),square(X,Y),no),
	call(Unapproved),
	retract(Unapproved),
	Approved=downOrRightNeighbor(square(X,YM2),square(X,Y),yes),
	assert(Approved).
%commit P once. always succeds
doOnce(P):- call(P),!.
doOnce(_).	

%decide what to do with the character Char located in (X,Y) and commit it.
handleSepRowChar(Char,X,Y):-
	Char=' ', 
	YP1 is Y+1,
	YM1 is Y-1,
	SUp=square(X,YM1),SDown=square(X,YP1),
	(call(SUp),
	assert(downOrRightNeighbor(SUp,SDown,no)),!;true )
	;
	Char = '-'.
	
%Char is the next char in the input
getChar(Char):-
	getCharCode(CharCode),atom_chars(Char,[CharCode]).
%CharCode is the code of the next char in the input
getCharCode(CharCode):-
	(\+ eof),get0(CharCode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%engine:
%marks alls the squares in the list Path by adding them to the DB as facts of onPath(Square).
markPath(Path):-
	forall( member(Node,Path),assert(onPath(Node)) ).

%moves function, true if there is a passage between squares N and M,the move cost is always 1.
s(N,M,1):- passage(N,M).
%heuristic function. H is the cost from N to the goal
h(N,H):- 
	N = square(X1,Y1),
	goal(square(X2,Y2)),
	DX is X2 -X1,
	DY is (Y2 -Y1)/2,
	H is DX + DY+1.
%returns the minimum of X,Y in Z
min(X,Y,X):- X=<Y,!.
min(_,Y,Y).

%Solution is a list containing the solution of the maze(as a square sequence).
engineSolve(Solution):- origin(Start),bestfirst(Start,Solution).

%%%%%%%%%%%%%%%%%%%%%%%
%documentation for bestfirst and sub predicates are in the book,figure 12.3 page 285.

bestfirst(Start,Solution):-
	expand([],l(Start,0/0),9999,_,yes,Solution).

%expand(Path,Tree,Bound,Tree1,Solved,Sol)
expand(P,l(N,_),_,_,yes,[N|P]):- goal(N),!. %added cut

expand(P,l(N,F/G),Bound,Tree1,Solved,Sol):-
	F=<Bound,
	( bagof(M/C,( s(N,M,C),\+ member(M,P) ),Succ),
	!,
	succlist(G,Succ,Ts),
	bestf(Ts,F1),
	expand(P,t(N,F1/G,Ts),Bound,Tree1,Solved,Sol)
	;
	Solved=never
	).

expand(P,t(N,F/G,[T|Ts]),Bound,Tree1,Solved,Sol):-
	F=<Bound,
	bestf(Ts,BF),min(Bound,BF,Bound1),
	expand([N|P],T,Bound1,T1,Solved1,Sol),
	continue(P,t(N,F/G,[T1|Ts]),Bound,Tree1,Solved1,Solved,Sol).

expand(_,t(_,_,[]),_,_,never,_):- !.

expand(_,Tree,Bound,Tree,no,_):-
	f(Tree,F),F>Bound.

continue(_,_,_,_,yes,yes,Sol):-!. %added cut

continue(P,t(N,F/G,[T1|Ts]),Bound,Tree1,no,Solved,Sol):-!, %added cut
	myinsert(T1,Ts,NTs),
	bestf(NTs,F1),
	expand(P,t(N,F1/G,NTs),Bound,Tree1,Solved,Sol).

continue(P,t(N,F/G,[_|Ts]),Bound,Tree1,never,Solved,Sol):- !, %added cut
	bestf(Ts,F1),
	expand(P,t(N,F1/G,Ts),Bound,Tree1,Solved,Sol).

succlist(G0,[N/C|NCs],Ts):- !, %added cut and changed order of predicates to make common first
	G is G0 +C,
	h(N,H),
	F is G+H,
	succlist(G0,NCs,Ts1),
	myinsert(l(N,F/G),Ts1,Ts).
succlist(_,[],[]).

myinsert(T,Ts,[T|Ts]):-
f(T,F),bestf(Ts,F1),
F=<F1,!.

myinsert(T,[T1|Ts],[T1|Ts1]):-
	myinsert(T,Ts,Ts1).

f(l(_,F/_),F):-!. %added cut
f(t(_,F/_,_),F).

bestf([T|_],F):- !, %added cut
	f(T,F).
bestf([],9999).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%output:

%writes the solution of the maze in InFileName into OutfileName
writePath(InFileName,OutFileName):-
	see(InFileName),
	tell(OutFileName),
	\+ writePathHelp(0,1),
	close(OutFileName),
	close(InFileName).

%help predicate for writePath
writePathHelp(X,Y):-
	getCharCode(CharCode),
	putCharOrStar(X,Y,CharCode),
	nextIndexes(CharCode,X,Y,X1,Y1),
	writePathHelp(X1,Y1).
%nextIndexes(CharCode,X,Y,X1,Y1) returns the next indexes in X1,Y1 based on the current indexes X,Y and the code of the last read character .
nextIndexes(NLCode,_,Y,0,Y1):- atom_chars('~J',[NLCode]),Y1 is Y+1,!. 
nextIndexes(_,X,Y,X1,Y):- X1 is X +1.

%prints a star(*) if square(X,Y) is on the solution path. Otherwise prints the character matching the code CharCode.
putCharOrStar(X,Y,CharCode):- 
	atom_chars(Char,[CharCode]),
	Char \= 's',
 	Char \= 't' ,
	onPath(square(X,Y)),
	atom_chars('*',[StarCode]),
	put(StarCode),!.

putCharOrStar(_,_,CharCode):- put(CharCode),!.
%retracts all predicates in the form P (P=F(X1,X2,..,Xn))
retractAll(P):- forall(P,retract(P)).

