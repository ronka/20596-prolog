found_on(bert,blood).
found_on(alan,mud).
found_on(jon,choclate).
found_on(barbara,choclate).
found_on(suzan,blood).
found_on(suzan,pudra).

have(bert,wooden_leg).
have(jon,pistol).
have(alan,rod).
have(barbara,scissors).

works_like(wooden_leg,club).
works_like(rod,club).
works_like(scissors,knife).

romance(jon,barbara).
romance(bert,barbara).
romance(jon,suzan).

murdered(susan,club).



killer(Person):-
    suspect(Person),
    murdered(Victim,_),
    found_on(Person,Material),
    found_on(Victim,Material).



% Helper relations

have_romance(P1,P2):-
    romance(P1,P2)
    ;
    romance(P2,P1).

suspect(Person):-
    % if have an item similar to to the killing item
    have(Person,Item),
    works_like(Item,KillingItem),
    murdered(_,KillingItem)
    ; % if killed with the same item
    have(Person,Item),
    murdered(_,KillingItem),
    Item = KillingItem
    ; % if have romance
    murdered(Victim,_),
    have_romance(Person,Victim)
    ; % if have romance with someone who is in a romance with a victim
    murdered(Victim,_),
    have_romance(Person,X), have_romance(X,Victim), X\=Victim.
