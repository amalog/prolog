:- use_module(library(amalog), []).

:- use_module(library(tap)).

small :-
    P = amalog{
        foo: clauses{
            1: clause{
                head: foo{
                    1: hi,
                    2: bye
                },
                body: goals{}
            }
        }
    },
    amalog:program(codes(Cs), P),
    Cs = `foo hi bye\n`.
