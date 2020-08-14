
knapsack(Items,Space,Amount,Profit):-
	% get all the possible options to order a bag inside ResultList
	bagof(
        [Profit1,Amount1], % template
        build_knapsack(Items,Space,Amount1,Profit1), % build a valid knapsack
        ResultList % store here
	),
	max(ResultList,Profit,Amount). % get maximum
	
%%%%% Helpers

% break down an triplet ( name-size-value ) down
item(Term,Name,Size,Value):-
	Term =.. [_|Args],
	Args = [H,Value], 
	H =.. [_|[Name,Size]].


% build a valid knapsack
build_knapsack([Item|ItemsTail],Space,[Amount|AmountTail],Profit):-
	% take the item
	Space > 0,
	item(Item,_,Size,Value),
	TempSpace is Space - Size,
	build_knapsack([Item|ItemsTail],TempSpace,[TempAmount|AmountTail],TempProfit),
	Profit is TempProfit + Value,
	Amount is TempAmount + 1
    ;
    % dont take the item
	build_knapsack(ItemsTail,Space,AmountTail,Profit),
	Amount = 0.

build_knapsack([],Space,[],0):- Space >= 0 .


% get tuple with max profit
max([[Profit,Amount]|T],ProfitRes,AmountRes):-
	max(T,MaxProfit,MaxAmount),
	(
        Profit >= MaxProfit,
        AmountRes = Amount,
        ProfitRes = Profit
	;
        Profit < MaxProfit,
        AmountRes = MaxAmount,
        ProfitRes = MaxProfit
	).
max([[Profit,Amount]|[]],Profit,Amount).
max([],_,_):- fail.