:- use_module(library(amalog), []).

:- use_module(library(tap)).

facts :-
    bagof( T, amalog:read(file("t/samples/facts.ama"),T), Terms),
    Terms == [ language(amalog)
             , language(prolog)
             , language('scheme-like?')
             ].

'camelcase atom prohibited'(fail) :-
    amalog:read(codes(`camelCase`),_).

'underscore separated atom prohibited'(fail) :-
    amalog:read(codes(`underscore_separated`),_).
