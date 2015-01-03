:- use_module(library(amalog), []).

:- use_module(library(tap)).

facts :-
    amalog:program(file("t/samples/facts.ama"),P),
    P == amalog{
        language: clauses{
            1: clause{
                head: language{1: amalog},
                body: goals{}
            },
            2: clause{
                head: language{1: prolog},
                body: goals{}
            },
            3: clause{
                head: language{1: 'scheme-like?'},
                body: goals{}
            }
        }
    }.

hello :-
    amalog:program(file("t/samples/hello.ama"),P),
    P == amalog{
        hello: clauses{
            1: clause{
                head: hello{1: pal},
                body: goals{
                    1: dear{
                        1: friend{1: of, 2: mine}
                    }
                }
            },
            2: clause{
                head: hello{1: you, 2: guys},
                body: goals{
                    1: 'say!'{1: "dearly beloved"},
                    2: 'be-sure'{1: "double ticks ` are OK"}
                }
            }
        },
        foo: clauses{
            1: clause{
                head: foo{1: bar, 2: baz},
                body: goals{1: do{1: stuff} }
            },
            2: clause{
                head: foo{1:var('_'), 2:var('_')},
                body: goals{}
            }
        }
    }.

'csv-munge' :-
    amalog:program(file("t/samples/csv-munge.ama"), P),
    P == amalog{
        main: clauses{
            1: clause{
                head: main{},
                body: goals{
                    1: handle{1: err},
                    2: 'pipe-all'{
                        1: 'csv-read'{
                            1: string_double{1:"secondary-notes.csv"},
                            columns: col
                        },
                        2: munge{},
                        3: 'csv-write'{
                            1: string_double{1: "secondary-munged.csv"}
                        }
                    }
                }
            }
        }
    }.

'camelcase atom prohibited'(fail) :-
    amalog:program(codes(`camelCase\n`),_).

'underscore separated atom prohibited'(fail) :-
    amalog:program(codes(`underscore_separated\n`),_).

'tags may not have a trailing dash'(fail) :-
    amalog:program(codes(`hello-\n`),_).

'keys may not have a trailing dash'(fail) :-
    amalog:program(codes(`main args-: bar\n`),_).
