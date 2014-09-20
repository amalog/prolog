:- module(amalog_dcg, [term//1]).

:- use_module(library(struct)).

:- structure state(indent_level:integer=0).

term(Term) -->
    { exists(state, State) },
    { defaults(State) },
    fact(State,Term).


fact(State,Term) -->
    functor(State,Functor),
    at_least(1,argument(State),Args),
    ( nl,eos          % last fact in the file
    ; nl,followed_by(black)        % followed by another clause
    ; nl,nl,nl,followed_by(black)  % last fact in the predicate
    ),
    { Term =.. [Functor|Args] }.


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


% match as many Goal as possible
:- meta_predicate amalog_dcg:greedy(3,-,*,*).
greedy(Goal,[X|Xs]) -->
    call(Goal,X),
    !,
    greedy(Goal,Xs).
greedy(_,[]) -->
    "".


% followed_by(Goal) is true if Goal would match. Consumes nothing.
:- meta_predicate amalog_dcg:followed_by(//,*,*).
followed_by(Goal) -->
    \+ \+ Goal.


functor(State,Functor) -->
    { struct:indent_level(State,0) },
    token(State,Token),
    { atom_codes(Functor,Token) }.


argument(State,Arg) -->
    token(State,Token),
    { atom_codes(Arg,Token) }.


token(_State,Token) -->
    at_least(1,black,Codes),
    ( at_least(1,space,_)
    ; followed_by(nl)
    ),
    { string_codes(Token,Codes) }.



nl -->
    "\n".


eos([],[]).


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


black_char(C) :-
    \+ white_char(C),
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
