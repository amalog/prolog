#!/usr/bin/env pl

:- user:asserta(file_search_path(library,prolog)).

:- use_module(library(amalog), []).

main([Filename]) :-
    once( amalog:program(file(Filename),Program)
        ; throw("Unable to parse program")
        ),
    writeq(Program),
    nl.
