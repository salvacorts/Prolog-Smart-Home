:- dynamic state/4.

/* Try the following queries:
?- ask([what, is, a, location], A).
?- ask([what, is, a, object], A).
?- ask([what, is, equiped, on, kitchen], A).
?- ask([what, is, action, of, tv], A).
?- ask([what, is, a, turned, on, object], A).
?- ask([what, is, the, volume, of, speaker], A, living_room).
*/

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

% Try:
%?- noun_phrase([a,spanish,speaking,country],T1,I1).
%?- noun_phrase([a,country,that,borders,chile],T2,I2).
%?- noun_phrase([a,spanish,speaking,country,that,borders,chile],T3,I3).

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
% We may add something here like is on, or temperature, or w/e
%adj([large | T],T,Obj) :- large(Obj).
%adj([Lang,speaking | T],T,Obj) :- speaks(Obj,Lang).
adj([is, turned, X | T], T, Obj, Location) :- action(Obj, power), state(Location, Obj, power, X).
adj([the, Attr, of, Obj | T], T, Ans, Location) :- state(Location, Obj, Attr, Ans).

noun([location | T],T,Obj) :- location(Obj).
noun([object | T],T,Obj) :- object(Obj).
noun([X | T],T,X) :- location(X).
noun([X | T],T,X) :- object(X).

reln([equiped, on | T],T,O1,O2) :- equiped(O2,O1).
reln([action, of | T],T,O1,O2) :- action(O2,O1).

% See nl_numbera maybe we can run the actions with this?
% The idea is running something like "volume_up the speaker in living_room"
% The feedback should be a print statement like "living_room tv channel_up"
verb([do, X |T], T, singular, do) :- action(_, X).


% question(Question,QR,Object) is true if Query provides an answer about Object to Question
question(['is' | T0],T2,Obj, Location) :-
   noun_phrase(T0,T1,Obj, Location),
   mp(T1,T2,Obj, Location).
question(['what',is | T0], T1, Obj, Location) :-
   mp(T0,T1,Obj, Location).
question(['what',is | T0],T1,Obj, Location) :-
   noun_phrase(T0,T1,Obj, Location).
question(['what' | T0],T2,Obj, Location) :-
   noun_phrase(T0,T1,Obj, Location),
   mp(T1,T2,Obj, Location).
