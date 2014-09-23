:- module(amalog_dcg, [ at_least//2
                      , at_least//3
                      , black//0
                      , black//1
                      , eos//0
                      , exactly//2
                      , exactly//3
                      , followed_by//1
                      , generous//2
                      , greedy//1
                      , greedy//2
                      , nl//0
                      , space//1
                      , term//1
                      , white//1
                      ]).

:- use_module(library(clpfd)).
:- use_module(library(struct)).

:- structure state(indent_level:integer=0).

term(Term) -->
    { exists(state, State) },
    { defaults(State) },
    fact(State,Term).


fact(State,Term) -->
    { struct:indent_level(State,0) },
    token(State,Functor),
    at_least(1,token(State),Args),
    ( nl,eos          % last fact in the file
    ; nl,followed_by(black)        % followed by another clause
    ; nl,nl,nl,followed_by(black)  % last fact in the predicate
    ),
    { Term =.. [Functor|Args] }.


:- meta_predicate at_least(+,3,*,*).
at_least(N,Goal) -->
    at_least(N,Goal,_).


% at_least(N,Goal,Matches) consumes at least N matches of Goal.
% after that, it consumes as much as possible.
:- meta_predicate at_least(+,3,?,*,*).
at_least(N0,Goal,[X|Xs]) -->
    { N0 > 0 },
    !,
    call(Goal,X),
    { N is N0 - 1 },
    at_least(N,Goal,Xs).
at_least(0,Goal,Xs) -->
    greedy(Goal,Xs).

% exactly(N,Goal) consumes exactly N matches of Goal.
:- meta_predicate exactly(+,3,*,*).
exactly(N,Goal) -->
    exactly(N,Goal,_).


% exactly(N,Goal,Matches) consumes exactly N matches of Goal.
:- meta_predicate exactly(+,3,?,*,*).
exactly(0,Goal,[]) -->
    \+ call(Goal,_).
exactly(N0,Goal,[X|Xs]) -->
    { N0 #> 0 },
    { N #= N0 - 1 },
    call(Goal,X),
    exactly(N,Goal,Xs).


% match as few Goal as possible
:- meta_predicate generous(3,-,*,*).
generous(_Goal,[]) -->
    [].
generous(Goal,[X|Xs]) -->
    call(Goal,X),
    generous(Goal,Xs).


:- meta_predicate greedy(3,*,*).
greedy(Goal) -->
    greedy(Goal,_).


% match as many copies of Goal as possible
:- meta_predicate amalog_dcg:greedy(3,-,*,*).
greedy(Goal,[X|Xs]) -->
    ( call(Goal,X) -> [] ),
    greedy(Goal,Xs).
greedy(_,[]) -->
    [].


% followed_by(Goal) is true if Goal would match. Consumes nothing.
:- meta_predicate amalog_dcg:followed_by(//,*,*).
followed_by(Goal) -->
    \+ \+ Goal.


token(_State,Token) -->
    at_least(1,black,Codes),
    ( at_least(1,space,_)
    ; followed_by(nl)
    ),
    { token_codes(Token,Codes) }.


%% token_codes(Token,Codes)
%
%  True if Token is represented in Amalog as Codes.
token_codes(Token,Codes) :-
    phrase(typed_token(Token),Codes).


typed_token(Token) -->
    at_least(1,atom_char,Codes),
    { atom_codes(Token,Codes) }.



nl -->
    "\n".


eos([],[]).


atom_char -->
    atom_char(_).

atom_char(C) -->
    [C],
    { atom_char(C) }.

black -->
    black(_).

black(Char) -->
    [Char],
    { black_char(Char) }.

space -->
    space(_).

space(C) -->
    [C],
    { space_char(C) }.

white(Char) -->
    [Char],
    { white_char(Char) }.


atom_char(C) :-
    black_char(C),
    C \== 0'_,
    \+ code_type(C,upper).

black_char(C) :-
    \+ white_char(C),
    \+ quote_char(C),
    \+ bookend_char(C).

bookend_char(0'().
bookend_char(0')).
bookend_char(0'[).
bookend_char(0']).
bookend_char(0'{).
bookend_char(0'}).

space_char(0'\s).

white_char(0'\s).
white_char(0'\n).

quote_char(0'').
quote_char(0'"). %"
quote_char(0'`).
