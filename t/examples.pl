:- use_module(library(amalog), []).

:- use_module(library(tap)).

facts :-
    amalog:program(file("t/samples/facts.ama"),P),
    P == amalog{
        language: [
            clause{
                head: language{1: amalog},
                body: []
            },
            clause{
                head: language{1: prolog},
                body: []
            },
            clause{
                head: language{1: 'scheme-like?'},
                body: []
            }
        ]
    }.

hello :-
    amalog:program(file("t/samples/hello.ama"),P),
    P == amalog{
        hello: [
            clause{
                head: hello{1: pal},
                body: [ dear{1: friend} ]
            },
            clause{
                head: hello{1: you, 2: guys},
                body: [ etc{} ]
            }
        ],
        foo: [
            clause{
                head: foo{1: bar, 2: baz},
                body: [do{1: stuff}]
            },
            clause{
                head: foo{1:'_', 2:'_'},
                body: []
            }
        ]
    }.

'camelcase atom prohibited'(todo) :-
    amalog:read(codes(`camelCase`),_).

'underscore separated atom prohibited'(todo) :-
    amalog:read(codes(`underscore_separated`),_).
