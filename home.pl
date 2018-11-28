% Prolog representation of a grammar to build a query for a database
%  This is not meant to be polished or lingustically reasonable, but purely to show what can be done

% This is slightly expanded code of Figure 13.11 in Section 13.6.6 of
% Poole and Mackworth, Artificial Intelligence: foundations of
% computational agents, Cambridge, 2017

% Copyright (c) David Poole and Alan Mackworth 2017. This program
% is released under GPL, version 3 or later; see http://www.gnu.org/licenses/gpl.html

% noun_phrase(T0,T4,Ind) is true if
%  T0 and T4 are list of words, such that
%        T4 is an ending of T0
%        the words in T0 before T4 (written T0-T4) form a noun phrase
%  Ind is an individual that the noun phrase is referring to

% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind) :-
   det(T0,T1,Ind),
   adjectives(T1,T2,Ind),
   noun(T2,T3,Ind),
   mp(T3,T4,Ind).

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
adjectives(T0,T2,Ind) :-
   adj(T0,T1,Ind),
   adjectives(T1,T2,Ind).
adjectives(T,T,_).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(T0,T2,Subject) :-
   reln(T0,T1,Subject,Object),
   noun_phrase(T1,T2,Object).
mp([that|T0],T2,Subject) :-
   reln(T0,T1,Subject,Object),
   noun_phrase(T1,T2,Object).
mp(T,T,_).

%%% DICTIONARY
% We may add something here like is on, or temperature, or w/e
%adj([large | T],T,Obj) :- large(Obj).
%adj([Lang,speaking | T],T,Obj) :- speaks(Obj,Lang).
adj([turned, X | T], T, Obj) :- action(Obj, power), state(Obj, _, power, X).

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
question(['is' | T0],T2,Obj) :-
   noun_phrase(T0,T1,Obj),
   mp(T1,T2,Obj).
question(['what',is | T0], T1, Obj) :-
   mp(T0,T1,Obj).
question(['what',is | T0],T1,Obj) :-
   noun_phrase(T0,T1,Obj).
question(['what' | T0],T2,Obj) :-
   noun_phrase(T0,T1,Obj),
   mp(T1,T2,Obj).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
   question(Q,[],A).

%%%%% The Database of Facts to be Queried

%% location(Location): Defines a location (room)
location(living_room).
location(kitchen).
location(corridor).
location(bedroom_1).
location(bedroom_2).

%% object(Object): Defines an object
object(light).
object(heating).
object(ac).
object(curtain).
object(tv).
object(speaker).
object(oven).
object(cofee_machine).
object(dish_washer).

%% equiped(Location, Object): Location has the Object
equiped(living_room, light).
equiped(living_room, heating).
equiped(living_room, ac).
equiped(living_room, curtain).
equiped(living_room, tv).
equiped(living_room, speaker).

equiped(kitchen, light).
equiped(kitchen, heating).
equiped(kitchen, oven).
equiped(kitchen, cofee_machine).
equiped(kitchen, dish_washer).

equiped(bedroom_1, light).
equiped(bedroom_1, heating).
equiped(bedroom_1, ac).
equiped(bedroom_1, curtain).
equiped(bedroom_1, tv).

equiped(bedroom_2, light).
equiped(bedroom_2, heating).
equiped(bedroom_2, curtain).

equiped(corridor, light).

%% action(Object, Action): The Object can perform an action
action(light, power). % power means it can be ON or OFF

action(curtain, power). % In this case power means closed or opened

action(heating, power).
action(heating, increase).
action(heating, decrease).

action(ac, power).
action(ac, increase).
action(ac, decrease).

action(tv, power).
action(tv, volume_up).
action(tv, volume_down).
action(tv, channel_up).
action(tv, channel_down).

action(speaker, power).
action(speaker, volume_up).
action(speaker, volume_down).
action(speaker, play).
action(speaker, pause).
% action(speaker, pause). maybe play song? but it has another argument 

action(oven, power).
action(oven, temp_up).
action(oven, temp_down).
% action(oven, set_timer). Again we would need another argument

action(dish_washer, power).
action(dish_washer, start).

action(cofee_machine, power).
action(cofee_machine, make). % make cofee

%% is_on(Location, Object): The Object in this Location is ON otherwise is off

%% state(Location, Object, attribute, value)
%state(living_room, light, power, on).
%state(kitchen, light, power, off).
%state(bedroom_1, light, power, off).
%state(bedroom_2, light, power, on).
%state(corridor, light, power, off).

%state(living_room, heating, power, on).
%state(living_room, heating, temperature, 20).

%state(kitchen, heating, power, off).
%state(kitchen, heating, temperature, 20).

%state(bedroom_1, heating, power, on).
%state(bedroom_1, heating, temperature, 20).

%state(bedroom_2, heating, power, on).
%state(bedroom_2, heating, temperature, 20).

%state(corridor, heating, power, off).
%state(corridor, heating, temperature, 20).

%state(living_room, ac, power, off).
%state(living_room, ac, temperature, 20).

%state(bedroom_1, ac, power, off).
%state(bedroom_1, ac, temperature, 20).

%state(living_room, curtain, power, off).
%state(bedroom_1, curtain, power, off).
%state(bedroom_2, curtain, power, on).

%state(living_room, tv, power, on).
%state(living_room, tv, channel, 7).
%state(living_room, tv, volume, 18).

%state(bedroom_1, tv, power, on).
%state(bedroom_1, tv, channel, 10).
%state(bedroom_1, tv, volume, 12).

%state(living_room, speaker, power, on).
%state(living_room, speaker, volume, 11).
%state(living_room, speaker, playing, true).

%state(kitchen, oven, power, off).
%state(kitchen, oven, temperature, 200).

%state(kitchen, cofee_machine, power, off).

%state(kitchen, dish_washer, power, off).

/* Try the following queries:
?- ask([what, is, a, location], A).
?- ask([what, is, a, object], A).
?- ask([what, is, equiped, on, kitchen], A).
?- ask([what, is, a, object, that, equiped, on, kitchen], A).
?- ask([what, is, action, of, tv], A).
?- ask([what, is, a, turned, on, object], A).
?- ask([what, is, a, turned, off, object], A).
*/


% To get the input from a line:

q(Ans) :-
   write("Ask me: "), flush_output(current_output),
   readln(Ln),
   question(Ln,End,Ans),
   member(End,[[],['?'],['.']]).

/*
?- q(Ans).
Ask me: What is a country that borders chile?
Ans = argentina ;
Ans = peru ;
Ans = brazil ;
false.

?- q(Ans).
Ask me: What is the capital of a spanish speaking country that borders chile?
Ans = buenos_aires ;
Ans = lima ;
false.

Some more questions:
What is next to chile?
Is brazil next to chile?
What is a country that borders a country that borders chile.
What is borders chile?
What borders chile?

*/
