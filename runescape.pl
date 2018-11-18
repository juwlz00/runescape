

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% Make a call to the Game API and build the knowledge base dynamically
steam(Dict, R, S) :-
  setup_call_cleanup(
    http_open('https://api.myjson.com/bins/ymjhu', In, []),
    json_read_dict(In, Dict),
    close(In)
  ),
  dict_x(Dict, R),
  dict_y(R,S).

% Parse the JSON file retrieved from the API call and build knowledge base dynamically
dict_x(X, X.apps).
dict_y([H|R], H) :-
  % Remove the quotations from the JSON values
  term_to_atom(name,H.name),
  term_to_atom(difficulty,H.difficulty),
  term_to_atom(length,H.length),
  term_to_atom(questpoints,H.qp),
  % Build knowledge base
  assert(quest(name)),
  assert(quest_difficulty(name,difficulty)),
  assert(quest_length(name,length)),
  assert(quest_points(name,questpoints)),
  dict_y(R, _).

% Build the knowledge base first whenever the project is made before allowing
% users to make queries
%:- initialization(steam(_,_,_)).
