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

% Build the knowledge base first whenever the project is made before allowing
% users to make queries
%:- initialization(steam(_,_,_)).

% Helper functions to complete specific queries
% similarDifficulty(X,Y) :-
%  setof((X,Y), B^(quest_difficulty(X,B), quest_difficulty(Y,B), \+X=Y), &&Quest),
%  member((X,Y), Quest).
