:- dynamic state/4.

c(Location):-
    write("command me: "),flush_output(current_output),
    readln(Ln),
    command(Ln,End,V,N,_,L,P,N2),
    member(End,[[],['.']]),
    exe(V,N,P,N2,Location,L).

command(L1,L2,V,N,LP,L,P,N2):-
    verb_phrase(L1,L2,V,N,LP,L,P,N2),
    valid_command(V,N,LP,L,P,N2).

valid_command(activate, Obj, empty, empty, empty, empty):- 
    action(Obj,power).	

valid_command(activate, Obj, LP, L, empty, empty):- 
    action(Obj,power) , 
    member(LP, [in,of]), location(L).

valid_command(deactivate, Obj, empty, empty, empty, empty):- 
    action(Obj,power).	

valid_command(deactivate, Obj, LP, L, empty, empty):- 
    action(Obj,power) , 
    member(LP, [in,of]), location(L).

valid_command(equip, L, empty, empty, with, Obj):- 
    location(L), 
    object(Obj).

valid_command(drop, Obj, LP, L, empty, empty):- 
    location(L), 
    object(Obj), 
    member(LP, [in,of]).

valid_command(set, volume, of, Obj, to, Size):- 
    action(Obj, volume_up), 
    number(Size). 

valid_command(set, channel, of, Obj, to, Size):- 
    action(Obj, channel_up), 
    number(Size). 

valid_command(set, Obj, to, channel_num(Num), empty, empty):- 
    action(Obj, channel_up), 
    number(Num).

valid_command(set, Obj, empty, empty, to, Size):- 
    action(Obj, increase), 
    number(Size).

valid_command(set, Obj, LP, L, to, Size):- 
    action(Obj, increase), 
    number(Size), 
    member(LP, [in, of]), 
    location(L).

valid_command(play, music, empty, empty, empty, empty).

valid_command(play, Song, empty, empty, empty, empty):- 
    song(Song).

valid_command(pause, music, empty, empty, empty, empty).


exe(activate,Obj,empty,empty,_, L):- 
    location(L), 
    object(Obj), 
    equiped(L,Obj),
    retractall(state(L,Obj,power,_)),
    assertz(state(L,Obj,power,on)).

exe(activate,Obj,empty,empty,Location, empty):- 
    location(Location), 
    object(Obj), 
    equiped(Location,Obj), 
    retractall(state(Location,Obj,power,_)), 
    assertz(state(Location,Obj,power,on)).

exe(deactivate,Obj,empty,empty,_, L):- 
    location(L), 
    object(Obj), 
    equiped(L,Obj), 
    retractall(state(L,Obj,power,_)), 
    assertz(state(L,Obj,power,off)).
	
exe(deactivate,Obj,empty,empty,Location, empty):- 
    location(Location), 
    object(Obj), 
    equiped(Location,Obj), 
    retractall(state(Location,Obj,power,_)), 
    assertz(state(Location,Obj,power,off)).
	
exe(equip,Location, with, Obj, L, empty):- 
    location(L), 
    object(Obj), 
    assertz(equiped(Location, Obj)).
	
exe(drop, Obj, empty, empty, _, L):- 
    location(L), 
    object(Obj), 
    retractall(equiped(L,Obj)).
	
exe(drop, Obj, empty, empty, Location, L):- 
    location(Location), 
    object(Obj), 
    retractall(equiped(L,Obj)).
	
exe(drop, Obj, empty, empty, Location, empty):- 
    location(Location), 
    object(Obj), 
    retractall(equiped(Location,Obj)).
	
exe(set, volume, to, Size, Location, Obj):- 
    location(Location), 
    equiped(Location,Obj), 
    state(Location,Obj,power,on), 
    retractall(state(Location,Obj,volume,_)), 
    assertz(state(Location,Obj,volume,Size)).

exe(set, channel, to, Size, Location, Obj):- 
    location(Location), 
    equiped(Location,Obj), 
    state(Location,Obj,power,on), 
    retractall(state(Location,Obj,channel,_)), 
    assertz(state(Location,Obj,channel,Size)).
	
exe(set, Obj, empty, empty, Location, channel_num(Num)):-
    location(Location),
    equiped(Location,Obj),
	state(Location, Obj, power, on),
	retractall(state(Location,Obj,channel,_)),
	assertz(state(Location,Obj,channel,Num)).

exe(set, Obj, to, Size, _, L):- 
    location(L), 
    object(Obj), 
    action(Obj, increase), 
    number(Size), 
    equiped(L, Obj), 
    state(L, Obj, power, on), 
    retractall(state(L,Obj,temperature,_)), 
    assertz(state(L,Obj,temperature,_)). 

exe(set, Obj, to, Size, Location, empty):- 
    location(Location), 
    object(Obj), 
    action(Obj, increase), 
    number(Size), 
    equiped(Location, Obj), 
    state(Location, Obj, power, on), 
    retractall(state(Location,Obj,temperature,_)), 
    assertz(state(Location,Obj,temperature,Size)). 
	
exe(set, oven, to, Size, _, empty):-
    number(Size),
    equiped(kitchen, oven),
    state(kitchen, oven, power, on),
	retractall(state(Location,Obj,temperature,_)),
	assertz(state(Location,Obj,temperature,Size)). 

exe(play, music, empty, empty, Location, empty):-
    location(Location),
    equiped(Location, speaker),
    retractall(state(Location,speaker,power,_)),
    retractall(state(Location,speaker,playing,_)),	
    retractall(state(Location,speaker,song_played,_)),
    assertz(state(Location, speaker, power, on)),
    assertz(state(Location, speaker, playing, true)),
    assertz(state(Location, speaker, song_played, default_song)).
	
exe(pause, music, empty, empty, Location, empty):-
    location(Location),
    equiped(Location, speaker),
	state(Location,speaker,power,on),
    retractall(state(Location,speaker,playing,_)),	
    assertz(state(Location, speaker, playing, false)).

exe(play, Song, empty, empty, Location, empty):-
    location(Location),
    equiped(Location, speaker),
	song(Song),
    retractall(state(Location,speaker,power,_)),
    retractall(state(Location,speaker,playing,_)),	
    retractall(state(Location,speaker,song_played,_)),
    assertz(state(Location, speaker, power, on)),
    assertz(state(Location, speaker, playing, true)),
    assertz(state(Location, speaker, song_played, Song)).

% Prolog representation of a context-free grammar for a very restricted subset of English

% This is slightly expanded code of Figures 13.7 & 13.8 in Section 13.6.1 of
% Poole and Mackworth, Artificial Intelligence: foundations of
% computational agents, Cambridge, 2017.

% Copyright (c) David Poole and Alan Mackworth 2017. This program
% is released under GPL, version 3 or later; see http://www.gnu.org/licenses/gpl.html

% a sentence is a noun_c phrase followed by a verb phrase.
sentence(L0,L2) :- 
   noun_c_phrase(L0,L1), 
   verb_phrase(L1,L2).

% a noun_c phrase is a determiner followed by adjectives followed
% by a noun_c followed by an optional prepositional phrase.
noun_c_phrase(L0,L4,N,LP,L) :- 
   det(L0,L1), 
   adjectives(L1,L2), 
   noun_c(L2,L3,N), 
   pp(L3,L4,LP,L).

% Try:
%?- noun_c_phrase([the,student,passed,the,computer,science,course],R).
%?- noun_c_phrase([the,new,computer,science,student,passed,the,course],R).
%?- noun_c_phrase([the,computer,science,course,with,a,computer],R).

% an optional noun_c phrase is either nothing or a noun_c phrase
opt_noun_c_phrase(L,L,empty,empty,empty).
opt_noun_c_phrase(L0,L1,N,LP,L) :-
   noun_c_phrase(L0,L1,N,LP,L).

% a verb phrase is a verb followed by a noun_c phrase and an optional pp
verb_phrase(L0,L3,V,N,LP,L,P,N2) :- 
   verb(L0,L1,V), 
   opt_noun_c_phrase(L1,L2,N,LP,L), 
   pp(L2,L3,P,N2).

% an optional prepositional phrase is either
% nothing or a preposition followed by a noun_c phrase
pp(L,L,empty, empty).
pp(L0,L2,P,N) :-
   preposition(L0,L1,P),
   noun_c_phrase(L1,L2,N,_,_).

% adjectives is a sequence of adjectives
adjectives(L,L).
adjectives(L0,L2) :-
    adj(L0,L1),
    adjectives(L1,L2).

% dictionary
det([a | L], L).
det([the | L], L).
det([my | L], L).
det(L,L).

noun_c([light | L], L, light).
noun_c([lights | L], L, light).
noun_c([tv | L], L, tv).
noun_c([television | L], L, tv).
noun_c([ac | L], L, ac).
noun_c([air, conditioner | L], L, ac).
noun_c([volume | L], L, volume).
noun_c([speakers | L], L, speaker).
noun_c([speaker | L], L, speaker).
noun_c([music | L], L, music).
noun_c([oven | L], L, oven).
noun_c([dish,washer | L], L, dish_washer).
noun_c([Location | L], L, Location):- location(Location).
noun_c([Number | L], L, Number):- number(Number).
noun_c([channel, Num | L], L, channel_num(22)):- number(Num).
noun_c(S, L, Song):- append(Song, L, S), song(Song).

adj([practical | L],L).
adj([new | L],L).
adj([computer, science | L],L).

verb([turn, on | L],L, activate).
verb([activate | L],L, activate).
verb([open | L], L, activate).
verb([turn, off | L],L, deactivate).
verb([deactivate | L], L, deactivate).
verb([strat | L], L, start).
verb([equip | L], L, equip).
verb([drop | L], L, drop).
verb([set | L], L, set).
verb([play | L], L, play).
verb([pause | L], L, pause).
verb([start | L], L, start).

preposition([in | L],L, in).
preposition([on | L],L, on).
preposition([of | L],L, of).
preposition([with | L], L, with).
preposition([to | L], L, to).

% Example Query:
%?- sentence([the,new,computer,science,student,passed,the,course],R).
%?- sentence([the,student,passed,the,computer,science,course,with,a,computer],R).
