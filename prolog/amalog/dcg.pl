:- module(amalog_dcg, [ program//1 ]).

:- use_module(library(dcg/basics), [string_without//2]).
:- use_module(library(clpfd)).
:- use_module(library(delay)).

:- use_module(library(dcg_util)).


:- multifile delay:mode/1.
delay:mode(system:string(ground)).

delay:mode(amalog_dcg:map(nonvar,list,_)).
delay:mode(amalog_dcg:map(nonvar,_,list)).

delay:mode(amalog_dcg:backtick_count(ground,_)).

:- [dcg/binary].
:- [dcg/dict].


% alias for maplist/3 that's not subject to apply_macros expansion.
% this allows us to use delay/1
:- meta_predicate map(2,?,?).
map(F,Xs,Ys) :-
    maplist(F,Xs,Ys).


program(Program) -->
    { delay(dict_pairs(Program, amalog, Pairs)) },
    { delay(map(predicate_pair, Predicates, Pairs)) },
    list(predicate, predicate_separator, Predicates),
    nl.

predicate_pair(Clauses, Name-Dict) :-
    list_dict(clauses,Clauses,Dict),
    Clauses = [Clause|_],
    is_dict(Clause.head, Name).

predicate(Predicate) -->
    list(clause, clause_separator, Predicate).

predicate_separator -->
    nl,
    nl,
    nl.

clause(clause{head: Head, body: Body}) -->
    uniline_term(1,Head),
    ( term_separator(1),
      list(term(1), term_separator(1), Terms)
    ; { Terms=[] }
    ),
    { list_dict(goals,Terms,Body) }.

clause_separator -->
    nl,
    followed_by(black).


term(Level,Term) -->
    multiline_term(Level,Term).
term(Level,Term) -->
    uniline_term(Level,Term).

multiline_term(IndentLevel0,Term) -->
    word(Name),
    { succ(IndentLevel0, IndentLevel) },
    term_separator(IndentLevel),
    list(term(IndentLevel), term_separator(IndentLevel), Args),
    { list_dict(Name,Args,Term) }.

uniline_term(_IndentLevel,Term) -->
    { delay(list_dict(Name,List,Term)) },
    word(Name),
    ( word_separator,
      list(uniline_argument, word_separator, List)
    ; { List = [] }
    ).

uniline_argument(Term) -->
    "(",
    uniline_term(_,Term),
    ")".
uniline_argument(string_double{1:Bytes}) -->
    "\"",
    string_without(`"`, Codes),
    "\"",
    { string_codes(Bytes, Codes) }.
uniline_argument(Binary) -->
    { delay(string(Binary)) },
    binary(Binary).
uniline_argument(Term) -->
    word(Term).


term_separator(IndentLevel) -->
    nl,
    indent(IndentLevel).

word(Word) -->
    { delay(atom_codes(Word,Codes)) },
    at_least(1,black,Codes),
    !.

word_separator -->
    space.


% True if Atom is a key (ends with ":") whose value is Key.
is_key(Atom,Key) :-
    atom(Atom),
    atom_concat(Key,':',Atom).


nl -->
    "\n".


indent -->
    "    ".

indent(1) -->
    indent.
indent(Level1) -->
    { Level1 > 1 },
    indent,
    { succ(Level,Level1) },
    indent(Level).
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
    \+ quote_char(C),
    \+ bookend_char(C).

bookend_char(0'(). %'
bookend_char(0')). %'
bookend_char(0'[). %'
bookend_char(0']). %'
bookend_char(0'{). %'
bookend_char(0'}). %'

space_char(0'\s).

white_char(0'\s).
white_char(0'\n).

quote_char(0'').
quote_char(0'"). %"'
quote_char(0'`). %`'
