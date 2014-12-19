:- use_module(library(amalog), []).

:- use_module(library(tap)).

facts :-
    amalog:program(file("t/samples/facts.ama"),P),
    P == [ [ clause( language{1: amalog}, [] )
           , clause( language{1: prolog}, [] )
           , clause( language{1: 'scheme-like?'}, [] )
           ]
    ].

hello :-
    amalog:program(file("t/samples/hello.ama"),P),
    P == [ [ clause( hello{1: pal}
                   , [ dear{1: friend} ]
                   )
           , clause( hello{1: you, 2: guys}
                   , [ etc{} ]
                   )
           ]
         , [ clause( foo{1: bar, 2: baz}
                   , [ do{1: stuff} ]
                   )
           , clause( foo{1:'_', 2:'_'}
                   , []
                   )
           ]
    ].

'camelcase atom prohibited'(todo) :-
    amalog:read(codes(`camelCase`),_).

'underscore separated atom prohibited'(todo) :-
    amalog:read(codes(`underscore_separated`),_).
