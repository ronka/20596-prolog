:- dynamic square/2.
:- dynamic origin/1.
:- dynamic goal/1.

solve(InputFile,OutputFile):-
	% cleanDynamics,
	readInput(InputFile).
	% engineSolve(Path),
	% markPath(Path),
	% writePath(InputFile,OutputFile).

%Solution is a list containing the solution of the maze(as a square sequence).
engineSolve(Solution):- origin(Start),dfs([],Start,Solution).


%transforms the input in InFileName to the relevant facts for solving the maze.
readInput(InFileName):-
	see(InFileName),
	doOnce(readInputHelp(1)),
	% retractUnaprroved, 
	close(InFileName).

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
	Char\='~M',
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




%********************
% helpers
%********************


%commit P once. always succeds
doOnce(P):- call(P),!.
doOnce(_).

%Char is the next char in the input
getChar(Char):-
	getCharCode(CharCode),atom_chars(Char,[CharCode]).
%CharCode is the code of the next char in the input
getCharCode(CharCode):-
	(\+ eof),get0(CharCode).

dfs(Path,Node,[Node|Path]):- goal(Node).
dfs(Path,Node,Sol):-
	passage(Node,Node1),
	\+ member(Node1,Path),
	dfs([Node|Path],Node1,Sol).