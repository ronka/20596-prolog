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
	not(X=0,Char=' '),
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

%Solution is a list containing the solution of the maze(as a square sequence).
engineSolve(Solution):- origin(Start),dfs([],Start,Solution).
%dfs documentation is in the book(chapter 11).
dfs(Path,Node,[Node|Path]):- goal(Node).
dfs(Path,Node,Sol):-
	passage(Node,Node1),
	\+ member(Node1,Path),
	dfs([Node|Path],Node1,Sol).







markPath(Path):-
	forall( member(Node,Path),assert(onPath(Node)) ).


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

