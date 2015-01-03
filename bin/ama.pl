#!/usr/bin/env pl

:- user:asserta(file_search_path(library,prolog)).

:- use_module(library(amalog), []).
:- use_module(library(pprint), [print_term/2]).

main([Filename]) :-
    once( amalog:program(file(Filename),Program)
        ; throw("Unable to parse program")
        ),
    print_term(Program,[indent_arguments(4)]),
    nl.
