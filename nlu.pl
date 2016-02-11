
/******************* parser **********************/

what(Words, Ref) :- np(Words, Ref).

/* Noun phrase can be a proper name or can start with an article */

np([Name],Name) :- proper_noun(Name).


/* Q5: modified the next two statements to handle the article "the" */
np([Art|Rest], Who) :- article(Art), not Art = the, np2(Rest, Who).
np([the|Rest], Who) :- np2(Rest, Who), not (np2(Rest, Who2), not Who = Who2).

/* If a noun phrase starts with an article, then it must be followed
   by another noun phrase that starts either with an adjective
   or with a common noun. */

np2([Adj|Rest],Who) :- adjective(Adj,Who), np2(Rest, Who).
np2([Noun|Rest], Who) :- common_noun(Noun, Who), mods(Rest,Who).


/* Modifier(s) provide an additional specific info about nouns.
   Modifier can be a prepositional phrase followed by none, one or more
   additional modifiers.  */

mods([], _).
mods(Words, Who) :-
	appendLists(Start, End, Words),
	prepPhrase(Start, Who),	mods(End, Who).

prepPhrase([Prep|Rest], Who) :-
	preposition(Prep, Who, Ref), np(Rest, Ref).

/* Q5: modified the parser to handle "Ex. balance between X and Y" */
prepPhrase([between,Ref1,and,Ref2|Rest], What) :- 
	preposition(between, What, Ref1, Ref2), 
	np(Rest, What).	
	
prepPhrase([between,Ref1,and,Ref2|Rest], What) :- 
	preposition(between, What, Ref1, Ref2).	
	
	
appendLists([], L, L).
appendLists([H|L1], L2, [H|L3]) :-  appendLists(L1, L2, L3).



recursive(L1,L2)
/********************* Lexicon *************************/


/*article*/
article(a).
article(an).
article(the).
article(any).


/*Country*/
proper_noun(usa).
proper_noun(canada).


/*cities*/
proper_noun(belleville).
proper_noun(brampton).
proper_noun(guelph).
proper_noun(kenora).
proper_noun(kingston).
proper_noun(kitchener).
proper_noun(london).
proper_noun(markham).
proper_noun(orillia).
proper_noun(toronto).
proper_noun(vaughan).
proper_noun(losAngeles).
proper_noun(newyork).

/*banks*/
proper_noun(metro_credit_union).
proper_noun(bMO).
proper_noun(nova_Scotia).
proper_noun(bridgeWater).
proper_noun(imperial_Bank).
proper_noun(tireBank).
proper_noun(cFF).
proper_noun(alterna).
proper_noun(directCash).
proper_noun(equitable).
proper_noun(firstNation).
proper_noun(rbc).

common_noun(city,X) :- location(X,Country).
common_noun(man,X) :- person(X), sex(X,male).
common_noun(woman,X) :- person(X), sex(X,female).

common_noun(account,X) :- account(X,Name,Bank,Balance).
common_noun(bank,X) :- account(Number,Name,X,Balance).
common_noun(person,X) :- person(X).
common_noun(balance,X) :- account(Number,Name,Bank,X).
common_noun(owner,X) :- account(Number,X,Bank,Balance).

common_noun(american,X) :- lives(X,City), location(City,usa).
common_noun(canadian,X) :- lives(X,City), location(City,canada).


preposition(in,City,Country) :- location(City,Country).
preposition(in,Account,Bank) :- account(Account,Name,Bank,Balance).
preposition(in,Bank,Country) :- account(Number,Name,Bank,Balance), location(Bank,City), location(City,Country).
preposition(in,Account,Country) :- account(Account,Name,Bank,Balance), location(Bank,City), location(City,Country).

preposition(with,Name,Account) :- person(Name), account(Account,Name,Bank,Balance).
preposition(with,City,Account) :- account(Account,Name,Bank,Balance), location(Bank,City).
preposition(with,Bank,Account) :- account(Account,Name,Bank,Balance).
preposition(with,Account,Balance) :- account(Account,Name,Bank,Balance).


preposition(from, Name,City) :- person(Name), lives(Name,City).
preposition(from, Name, Country) :- person(Name), lives(Name,City), location(City,Country).

preposition(of,Balance,Account) :- account(Account,Name,Bank,Balance).
preposition(of,Account,Name) :- account(Account,Name,Bank,Balance).
preposition(of,Name,Account) :- account(Account,Name,Bank,Balance).
preposition(between,Balance,MinBal,MaxBal) :- account(Number,Name,Bank,Balance), number(MinBal), number(MaxBal), Balance >= MinBal, Balance =< MaxBal .


adjective(canadian,X) :- lives(X,City), location(City,canada). /*Ex: canadian person*/
adjective(canadian,X) :- account(Number,Name,X,Balance), location(X,City), location(City,canada). /* Ex: canadian bank */
adjective(american,X) :- lives(X,City), location(City,usa). 
adjective(american,X) :- account(Number,Name,X,Balance), location(X,City), location(City,usa).

adjective(large,X) :- account(X,Name,Bank,Balance), Balance > 10000 .
adjective(small,X) :- account(X,Name,Bank,Balance), Balance < 1000 .
adjective(medium,X) :- account(X,Name,Bank,Balance), Balance =< 10000, Balance >= 1000 .
adjective(local,X) :- account(Number,Name,X,Balance), location(X,City), location(City,canada).
adjective(foreign,X) :- lives(X,City), location(City,Country), not Country = canada .
adjective(male,X) :- person(X), sex(X,male).
adjective(female,X) :- person(X), sex(X,female).
adjective(old,X) :- created(X, Name, Bank,Year), Year < 2014.
adjective(recent,X) :- created(X, Name, Bank,2014).


/*********************** database ************************/


/* Person */

person(ann).
person(barry).
person(marisela).	
person(eliz).	
person(jena).	
person(ladonna).	
person(billy).	
person(amado).	
person(ester).	
person(ger).	
person(ed).
person(troy).
person(bob).
person(nancy).
person(jj).
person(jz).

sex(ann,female).
sex(barry,male).
sex(marisela,female).
sex(eliz,female).
sex(jena,female).
sex(ladonna,female).
sex(billy,male).
sex(amado,female).
sex(ester,female).
sex(ger,male).
sex(ed,male).
sex(troy,male).
sex(bob,male).
sex(nancy,female).
sex(jj,male).
sex(jz,male).

/* lives(Person,City) */
lives(ann,belleville).
lives(barry,scarborough).
lives(marisela,guelph).
lives(eliz,kenora).
lives(jena,kingston).
lives(ladonna,kitchener).
lives(billy,london).
lives(amado,markham).
lives(ester,orillia).
lives(geri,toronto).
lives(ed,vaughan).
lives(troy,losAngeles).
lives(bob,newyork).
lives(nancy,newyork).
lives(jj,richmondhill).
lives(jz,vaughan).

/* Location(X,C) were the city are */
location(belleville,canada).
location(brampton,canada).
location(guelph,canada).
location(kenora,canada).
location(kingston,canada).
location(kitchener,canada).
location(london,canada).
location(markham,canada).
location(orillia,canada).
location(toronto,canada).
location(vaughan,canada).
location(losAngeles,usa).
location(newyork,usa).
location(richmondhill,canada)

/* Location(X,C) where the city for banks are */
location(metro_credit_union,toronto).
location(bMO,scarborough).
location(nova_Scotia,guelph).
location(bridgeWater,kingston).
location(imperial_Bank,toronto).
location(tireBank,kitchener).
location(cFF,vaughan).
location(alterna,toronto).
location(directCash,belleville).
location(equitable,vaughan).
location(firstNation,kitchener).
location(rbc,richmondhill).
location(cibc,vaughan).

/* account(Number,Name,Bank,Balance) */
account(1,ann,mero_credit_union,25).
account(2,barry,bMO,30000).
account(3,marisela,nova_Scotia,2505).
account(4,eliz,bridgeWater,2505).
account(5,jena,imperial_Bank,2505).
account(6,ladonna,tireBank,250125).
account(7,billy,cFF,255).
account(8,amado,metro_credit_union,5000).
account(9,ester,directCash,255).
account(10,geri,equitable,205).
account(11,ed,firstNation,205).
account(12,troy,rbc,10).
account(13,bob,rbc,5000).
account(14,nancy,rbc,100000).

account(15,jj,rbc,1001).
account(16,jz,cibc,201).


/* created(Number,Name,Bank,Month,Year) */
created(1,ann,mero_credit_union,2001).
created(2,barry,bMO,1898).
created(3,marisela,nova_Scotia,1901).
created(4,eliz,bridgeWater,1900).
created(5,jena,imperial_Bank,1994).
created(6,ladonna,tireBank,1897).
created(7,billy,cFF,1879).
created(8,amado,metro_credit_union,1866).
created(9,ester,directCash,2004).
created(10,geri,equitable,2006).
created(11,ed,firstNation,2014).
created(12,troy,rbc,2010).
created(13,bob,rbc,2010).
created(14,nancy,rbc,2014).

created(15,jj,rbc,2009).
created(16,jz,cibc,1978).
