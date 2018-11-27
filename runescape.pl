start(Ans) :-
    write("What level are you?: "),flush_output(current_output),
    readln([Head|Lnlvl]),
	write("Are you a member?: y/n "),flush_output(current_output),
    readln(Lnmem),
	assert(user_level(Head)),
	assert(user_member(Lnmem)).

% NLP based off Dr.Poole's geography.pl code
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind) :-
    det(T0,T1,Ind),
    adjectives(T1,T2,Ind),
    noun(T2,T3,Ind),
    mp(T3,T4,Ind).

% determiners
det([the | T],T,_).
det([a | T],T,_).
det(T,T,_).
	
% adjectives(T0,T1,Ind) is true if 
% T0-T1 is an adjective is true of Ind
adjectives(T0,T2,Ind) :-
    adj(T0,T1,Ind),
    adjectives(T1,T2,Ind).
adjectives(T,T,_).

% modifying phrase
mp(T0,T2,Subject) :-
    reln(T0,T1,Subject,Object),
    noun_phrase(T1,T2,Object).
mp([that|T0],T2,Subject) :-
    reln(T0,T1,Subject,Object),
    noun_phrase(T1,T2,Object).
mp(T,T,_).

% Dictionary
adj([Lang,difficulty | T],T,Obj) :- quest_difficulty(Obj,Lang).
adj([Lang,length | T],T,Obj) :- quest_length(Obj,Lang).

% nouns
noun([quest | T],T,Obj) :- quest(Obj).
noun([X | T],T,X) :- quest(X).
noun([X | T],T,X) :- quest_difficulty(A,X).

reln([unlocked, by | T],T,O1,O2) :- proceeds(O1,O2).
reln([required, for | T],T,O1,O2) :- preceeds(O1,O2).


% NLP v2

noun_phrase2(T0,T4,Obj,C0,C4) :-
    det2(T0,T1,Obj,C0,C1),
    adjectives2(T1,T2,Obj,C1,C2),
    noun2(T2,T3,Obj,C2,C3),
    mp2(T3,T4,Obj,C3,C4).

det2([the | T],T,_,C,C).
det2([a | T],T,_,C,C).
det2([as | T],T,_,C,C).
det2(T,T,_,C,C).

% Adjectives.
adjectives2(T,T,_,C,C).
adjectives2(T0,T2,Obj,C0,C2) :-
    adjective2(T0,T1,Obj,C0,C1),
    adjectives2(T1,T2,Obj,C1,C2).

adjective2([H,s | T],T,Obj,C,[language(Obj,H)|C]).

mp2(T,T,_,C,C).
mp2(T0,T2,O1,C0,C2) :-
    reln2(T0,T1,O1,O2,C0,C1),
    noun_phrase2(T1,T2,O2,C1,C2).
mp2([that|T0],T2,O1,C0,C2) :-
    reln2(T0,T1,O1,O2,C0,C1),
    noun_phrase2(T1,T2,O2,C1,C2).
mp2([that,is|T0],T2,O1,C0,C2) :-
    reln2(T0,T1,O1,O2,C0,C1),
    noun_phrase2(T1,T2,O2,C1,C2).
mp2([with|T0],T2,O1,C0,C2) :-
    reln2(T0,T1,O1,O2,C0,C1),
    noun_phrase2(T1,T2,O2,C1,C2).
  mp2([with, the|T0],T2,O1,C0,C2) :-
      reln2(T0,T1,O1,O2,C0,C1),
      noun_phrase2(T1,T2,O2,C1,C2).

% Noun
noun2([quest | T],T,Obj,C,[quest(Obj)|C]).
noun2([X | T],T,X,C,C) :- quest(X).

% relations
reln2([similar,difficulty,as | T],T,O1,O2,_,[sameDifficulty(O1,O2)]).
reln2([same,difficulty,as| T],T,O1,O2,_,[sameDifficulty(O1,O2)]).
reln2([similar,length | T],T,O1,O2,_,[sameLength(O1,O2)]).
reln2([same,length| T],T,O1,O2,_,[sameLength(O1,O2)]).
reln2([similar,quest,points | T],T,O1,O2,_,[sameQP(O1,O2)]).
reln2([same,quest,points| T],T,O1,O2,_,[sameQP(O1,O2)]).
reln2([similar,points | T],T,O1,O2,_,[sameQP(O1,O2)]).
reln2([same,points| T],T,O1,O2,_,[sameQP(O1,O2)]).

% question(Question,QR,Object) is true if Query provides an answer about Object to Question
question(['Is' | T0],T2,Obj) :-
    noun_phrase(T0,T1,Obj),
    mp(T1,T2,Obj).
question(['What',is | T0], T1, Obj) :-
    mp(T0,T1,Obj).
question(['What',is | T0],T1,Obj) :-
    noun_phrase(T0,T1,Obj).
question(['What' | T0],T2,Obj) :-
    noun_phrase(T0,T1,Obj),
    mp(T1,T2,Obj).
	
% complex questions 
question2([Is | T0],T1,Obj,C0,C1) :-
    noun_phrase2(T0,T1,Obj,C0,C1),
	mp2(T1,T2,Obj,C0,C1).
question2([What,is | T0],T1,Obj,C0,C1) :-
    noun_phrase2(T0,T1,Obj,C0,C1).
question2([What,is | T0],T1,Obj,C0,C1) :-
	mp2(T1,T2,Obj,C0,C1).
question2([What | T0],T1,Obj,C0,C1) :-
    noun_phrase2(T0,T1,Obj,C0,C1),
	mp2(T1,T2,Obj,C0,C1).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A).

ask2(Q,A) :-
    question2(Q,[],A,[],C),
    prove(C).

prove([]).
prove([H|T]) :-
    call(H),      
    prove(T).

% Helper functions to complete specific queries
 sameDifficulty(X,Y) :-
  setof((X,Y), Z^(quest_difficulty(X,Z), quest_difficulty(Y,Z), \+X=Y), Quest),
  member((X,Y), Quest).

 sameLength(X,Y) :-
  setof((X,Y), Z^(quest_length(X,Z), quest_length(Y,Z), \+X=Y), Quest),
  member((X,Y), Quest).
  
 sameQP(X,Y) :-
  setof((X,Y), Z^(quest_points(X,Z), quest_points(Y,Z), \+X=Y), Quest),
  member((X,Y), Quest).
  
% To get the input from a line:

q(Ans) :-
    write("Ask the guide: "),flush_output(current_output),
    readln(Ln),
    question(Ln,End,Ans),
    member(End,[[],['?'],['.']]).
	
quest(cooks_assistant).
quest(dragon_slayer).
quest(demon_slayer).
quest(the_knights_sword).
quest(black_knights_fortress).

difficulty(novice).
difficulty(intermediate).
difficulty(experienced).

quest_difficulty(cooks_assistant,novice).
quest_difficulty(dragon_slayer,intermediate).
quest_difficulty(demon_slayer,novice).
quest_difficulty(the_knights_sword,intermediate).
quest_difficulty(black_knights_fortress,novice).

length(short).
length(long).
length(medium).

quest_length(cooks_assistant,short).
quest_length(dragon_slayer,long).
quest_length(demon_slayer,medium).
quest_length(the_knights_sword,medium).
quest_length(black_knights_fortress,short).

quest_points(cooks_assistant,1).
quest_points(dragon_slayer,2).
quest_points(demon_slayer,3).
quest_points(the_knights_sword,1).
quest_points(black_knights_fortress,3).

preceeds(cooks_assistant,demon_slayer).
preceeds(cooks_assistant,dragon_slayer).
preceeds(demon_slayer,dragon_slayer).
preceeds(dragon_slayer,the_knights_sword).
preceeds(cooks_assistant,the_knights_sword).
preceeds(demon_slayer,the_knights_sword).

proceeds(B,A):-
	preceeds(A,B).
	
% Try
% ?- ask(['What',is,a,quest],A).
% ?- ask(['What',is,a,novice,difficulty,quest],A).
% ?- ask(['What',is,a,medium,length,quest],A).
% ?- ask(['What',is,a,novice,difficulty,short,length,quest],A).
% ?- ask(['What',is,a,novice,difficulty,quest,required,for,demon_slayer],A).
̀% ?- ask(['What',is,a,quest,unlocked,by,demon_slayer],A).
% ?- ask2(['What',is,a,quest,with,similar,difficulty,as,demon_slayer],A).
% ?- ask2(['What',is,a,quest,with,the,same,difficulty,as,demon_slayer],A).
% ?- ask2(['What',is,a,quest,with,similar,length,as,cooks_assistant],A).
% ?- ask2(['What',is,a,quest,with,the,same,quest,points,as,demon_slayer],A).
% ?- ask2(['What',is,a,quest,with,the,same,points,as,cooks_assistant],A).
