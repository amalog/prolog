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

:- [dcg/util].
:- [dcg/char].
:- [dcg/tag].
:- [dcg/key].
:- [dcg/binary].
:- [dcg/dict].
:- [dcg/term].


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
    { delay(list_dict(goals,Terms,Body)) },
    uniline_term(Head),
    ( term_separator(1),
      once(list(term(1), term_separator(1), Terms))
    ; { Terms=[] }
    ).


clause_separator -->
    nl,
    followed_by(black).


uniline_argument(Term) -->
    "(",
    uniline_term(Term),
    ")".
uniline_argument(string_double{1:Bytes}) -->
    "\"",
    string_without(`"`, Codes),
    "\"",
    { string_codes(Bytes, Codes) }.
uniline_argument(Binary) -->
    { delay(string(Binary)) },
    binary(Binary).
uniline_argument(Key) -->
    key(Key).
uniline_argument(Tag) -->
    tag(Tag).
uniline_argument(var('_')) -->
    "_".


indent -->
    "    ".

indent(1) -->
    indent.
indent(Level1) -->
    { Level1 > 1 },
    indent,
    { succ(Level,Level1) },
    indent(Level).


lowercase_word(Word) -->
    at_least(1,lowercase,Word).
