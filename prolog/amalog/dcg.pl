:- module(amalog_dcg, [ program//1 ]).

:- use_module(library(dcg/basics), [string_without//2]).
:- use_module(library(clpfd)).
:- use_module(library(delay)).


:- multifile delay:mode/1.
delay:mode(system:dict_pairs(nonvar,_,_)).
delay:mode(system:dict_pairs(_,_,list)).

delay:mode(amalog_dcg:map(nonvar,list,_)).
delay:mode(amalog_dcg:map(nonvar,_,list)).

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
    Clauses = [Clause|_],
    is_dict(Clause.head, Name),
    list_dict(clauses,Clauses,Dict).

predicate(Predicate) -->
    list(clause, clause_separator, Predicate).

predicate_separator -->
    nl,
    nl,
    nl.

clause(clause{head: Head, body: Body}) -->
    uniline_term(1,Head),
    ( term_separator(1),
      list(term(1), term_separator(1), Body)
    ; { Body=[] }
    ).

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
    word(Name),
    ( word_separator,
      list(uniline_argument, word_separator, List)
    ; { List = [] }
    ),
    { list_dict(Name,List,Term) }.

uniline_argument(Term) -->
    "(",
    uniline_term(_,Term),
    ")".
uniline_argument(string_double{1:Bytes}) -->
    "\"",
    string_without(`"`, Codes),
    "\"",
    { string_codes(Bytes, Codes) }.
uniline_argument(Term) -->
    word(Term).

term_separator(IndentLevel) -->
    nl,
    indent(IndentLevel).

word(Word) -->
    at_least(1,black,Codes),
    !,
    { atom_codes(Word, Codes) }.

word_separator -->
    space.


list_dict(Name, Values, Dict) :-
    once(phrase(list_dict_(1,Pairs),Values)),
    dict_pairs(Dict,Name,Pairs).

list_dict_(N,[Key-Value|Pairs]) -->
    [K,Value],
    { is_key(K,Key) },
    list_dict_(N,Pairs).
list_dict_(N0,[N0-Value|Pairs]) -->
    [Value],
    { succ(N0,N) },
    list_dict_(N,Pairs).
list_dict_(_,[]) --> [].


% True if Atom is a key (ends with ":") whose value is Key.
is_key(Atom,Key) :-
    atom(Atom),
    atom_concat(Key,':',Atom).


%% list(ElemDCG, SeparatorDCG, Elems)//
%
%  Describes a list in which the elements match ElemDCG and the
%  separators match SeparatorDCG. Elems is the list of elements found.
%  The set of patterns matched by ElemDCG and SeparatorDCG
%  should be disjoint.  Both DCG goals are called with one extra argument.
:- meta_predicate list(3,2,?,?,?).
list(ElemDCG, SepDCG, [Elem|Tail]) -->
    call(ElemDCG, Elem),
    ( call(SepDCG),
      !,
      list(ElemDCG, SepDCG, Tail)
    ; "",
      { Tail = [] }
    ).


% at_least//2 is like at_least//3 but ignores the specific matches found.
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
