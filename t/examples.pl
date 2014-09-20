:- use_module(library(amalog), []).

:- use_module(library(tap)).

facts :-
    bagof( T, amalog:read(file("t/samples/facts.ama"),T), Terms),
    Terms == [ language(amalog), language(prolog) ].
