found_on(bert,blood).
found_on(alan,mud).
found_on(jon,choclate).
found_on(barbara,choclate).
found_on(suzan,pudra).
found_on(suzan,blood).

have(bert,wooden_leg).
have(jon,pistol).
have(alan,rod).
have(barbara,scissors).

similar(wooden_leg,club).
similar(rod,club).
similar(scissors,knife).

romance(jon,barbara).
romance(bert,barbara).
romance(jon,suzan).

murdered(suzan,club).



killer(Person):-
    found_on(Victim,X),
    found_on(Person,X),
    murdered(Victim,_),
    suspect(Person).




% Helper relations

works_like(I1,I2):-
    similar(I1,I2)
    ;
    similar(I2,I1).

have_romance(P1,P2):-
    romance(P1,P2)
    ;
    romance(P2,P1).

suspect(Person):-
    % if have an item similar to to the killing item
    have(Person,X),
    works_like(X,Item),
    murdered(_,Item)
    ; % if have romance
    have_romance(Person,Victim),
    murdered(Victim,_)
    ; % if have romance with someone who is in a romance with a victim
    murdered(Victim,_),
    have_romance(Person,X), have_romance(X,Victim), Victim\=X.