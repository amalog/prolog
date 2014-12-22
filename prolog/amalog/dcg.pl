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
                      , program//1
                      , space//1
                      , term//2
                      , white//1
                      ]).

:- use_module(library(dcg/basics), [string_without//2]).
:- use_module(library(clpfd)).


program(Program) -->
    list(predicate, predicate_separator, Predicates),
    nl,
    { maplist(predicate_pair, Predicates, Pairs) },
    { dict_pairs(Program, amalog, Pairs) }.

predicate_pair(Clauses, Name-Clauses) :-
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
    list(word, word_separator, List),
    { list_term(List, Term) }.

term_separator(IndentLevel) -->
    nl,
    indent(IndentLevel).

word(string_double{1:Bytes}) -->
    "\"",
    !,
    string_without(`"`, Codes),
    "\"",
    { string_codes(Bytes, Codes) }.
word(Word) -->
    at_least(1,black,Codes),
    !,
    { atom_codes(Word, Codes) }.

word_separator -->
    space.


list_term([Name|Values], Term) :-
    list_dict(Name, Values, Term).

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
