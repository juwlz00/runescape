:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- dynamic(found/2).
% :- dynamic(similarDifficulty/2).

% Make a call to the Game API and build the knowledge base dynamically
steam(Dict, R, S) :-
  setup_call_cleanup(
    http_open('https://api.myjson.com/bins/zc1s6', In, []),
    json_read_dict(In, Dict),
    close(In)
  ),
  dict_x(Dict, R),
  dict_y(R,S).

% Parse the JSON file retrieved from the API call and build knowledge base dynamically
dict_x(X, X.apps).
dict_y([H|R], H) :-
  % Remove the quotations from the JSON values
  term_to_atom(Name,H.name),
  term_to_atom(Difficulty,H.difficulty),
  term_to_atom(Length,H.length),
  term_to_atom(Qp,H.questpoints),
  % Build knowledge base
  assert(quest(Name)),
  assert(quest_difficulty(Name,Difficulty)),
  assert(quest_length(Name,Length)),
  assert(quest_points(Name,Qp)),
  dict_y(R, _).

% Build the knowledge base first whenever the project is made before allowing users to make queries
:- initialization(steam(_,_,_)).

% NLP based off Dr.Poole's geography.pl code
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind) :-
    det(T0,T1,Ind),
    adjectives(T1,T2,Ind),
    noun(T2,T3,Ind),
    mp(T3,T4,Ind).
	
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

% nouns
noun([quest | T],T,Obj) :- quest(Obj).
noun([X | T],T,X) :- quest(X).

% relations
reln([similar,difficulty | T],T,O1,O2) :- similarDifficulty(O1,O2).
reln([same,difficulty| T],T,O1,O2) :- similarDifficulty(O1,O2).
reln([similar,length | T],T,O1,O2) :- similarLength(O1,O2).
reln([same,length| T],T,O1,O2) :- similarLength(O1,O2).
reln([similar,quest,points | T],T,O1,O2) :- similarQP(O1,O2).
reln([same,quest,points| T],T,O1,O2) :- similarQp(O1,O2).
reln([similar,points | T],T,O1,O2) :- similarQP(O1,O2).
reln([same,points| T],T,O1,O2) :- similarQp(O1,O2).

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

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A).

% Helper functions to complete specific queries
 similarDifficulty(X,Y) :-
  setof((X,Y), Z^(quest_difficulty(X,Z), quest_difficulty(Y,Z), \+X=Y), Quest),
  member((X,Y), Quest).

 similarLength(X,Y) :-
  setof((X,Y), Z^(quest_length(X,Z), quest_length(Y,Z), \+X=Y), Quest),
  member((X,Y), Quest).
  
 similarQP(X,Y) :-
  setof((X,Y), Z^(quest_points)(X,Z), quest_points(Y,Z), \+X=Y), Quest),
  member((X,Y), Quest).
  
% To get the input from a line:

q(Ans) :-
    write("Ask the guide: "),flush_output(current_output),
    readln(Ln),
    question(Ln,End,Ans),
    member(End,[[],['?'],['.']]).