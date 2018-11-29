:- dynamic state/4.

/* Example Questions
?- q(_, Ans).
Ask me: what is a location

?- q(_, Ans).
Ask me: what is a object

?- q(_, Ans).
Ask me: what is equiped on kitchen

?- q(_, Ans).
Ask me: what is action of tv

?- q(living_room, Ans).
Ask me: what is the volume of speaker

?- q(living_room, Ans).
Ask me: what is turned on

?- q(bedroom_1, Ans).
Aks me: what song is playing

?- q(living_room, Ans).
Ask me: what is the channel of tv

?- q(living_room, Ans).
Ask me: what is the temperature of ac

?- q(living_room, Ans).
Ask me: what is turned on in bedroom_1
*/

% ask(Q,A) gives answer A to question Q
ask(Q,A, L) :-
   question(Q,[],A, L).

q(Location, Ans) :-
   write("Ask me: "), flush_output(current_output),
   readln(Ln),
   question(Ln,End,Ans, Location),
   member(End,[[],['?'],['.']]).


% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind, Location) :-
   det(T0,T1,Ind),
   adjectives(T1,T2,Ind, Location),
   noun(T2,T3,Ind),
   mp(T3,T4,Ind, Location).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constraints.
det([the | T],T,_).
det([a | T],T,_).
det(T,T,_).

% adjectives(T0,T1,Ind) is true if 
% T0-T1 is an adjective is true of Ind
adjectives(T0,T2,Ind, Location) :-
   adj(T0,T1,Ind, Location),
   adjectives(T1,T2,Ind, Location).
adjectives(T,T,_, _).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(T0,T2,Subject, Location) :-
   reln(T0,T1,Subject,Object),
   noun_phrase(T1,T2,Object, Location).
mp(T0,T1,Subject, Location) :-
   adj(T0,T1,Subject,Location).
   %noun_phrase(T1,T2,Object, Location).
mp([that|T0],T1,Subject, Location) :-
   adj(T0,T1,Subject,Location).
   %noun_phrase(T1,T2,Object, Location).
mp([that|T0],T2,Subject, Location) :-
   reln(T0,T1,Subject,Object),
   noun_phrase(T1,T2,Object, Location).
mp(T,T,_, _).

%%% DICTIONARY
adj([turned, X | T], T, Obj, Location) :- action(Obj, power), state(Location, Obj, power, X).
adj([turned, X , in, L| T], T, Obj, _) :- action(Obj, power), state(L, Obj, power, X).
adj([the, Attr, of, Obj | T], T, Ans, Location) :- state(Location, Obj, Attr, Ans).
adj([the, Attr, of, Obj, in, L | T], T, Ans, _) :- state(L, Obj, Attr, Ans).
adj([song, is, playing | T], T, Ans, Location) :- state(Location, speaker, song_played, Ans).

noun([location | T],T,Obj) :- location(Obj).
noun([object | T],T,Obj) :- object(Obj).
noun([X | T],T,X) :- location(X).
noun([X | T],T,X) :- object(X).

reln([equiped, on | T],T,O1,O2) :- equiped(O2,O1).
reln([action, of | T],T,O1,O2) :- action(O2,O1).

% question(Question,QR,Object) is true if Query provides an answer about Object to Question
question(['is' | T0],T2,Obj, Location) :-
   noun_phrase(T0,T1,Obj, Location),
   mp(T1,T2,Obj, Location).
question(['what',is | T0], T1, Obj, Location) :-
   mp(T0,T1,Obj, Location).
question(['what',is | T0],T1,Obj, Location) :-
   noun_phrase(T0,T1,Obj, Location).
question(['what' | T0],T1,Obj, Location) :-
   mp(T0,T1,Obj, Location).
question(['what' | T0],T2,Obj, Location) :-
   noun_phrase(T0,T1,Obj, Location),
   mp(T1,T2,Obj, Location).
