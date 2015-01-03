#!/usr/bin/env pl

:- user:asserta(file_search_path(library,prolog)).

:- use_module(library(amalog), []).
:- use_module(library(pprint), [print_term/2]).
:- use_module(library(process)).

main([Filename]) :-
    once( amalog:program(file(Filename),Program)
        ; throw("Unable to parse program")
        ),
    with_pager(pretty_print(Program)),
    nl.


% pretty print an Amalog program AST
pretty_print(Program) :-
    print_term(
        Program,
        [
            indent_arguments(4),
            output(current_output)
        ]
    ).


% send output through the user's preferred pager
:- meta_predicate with_pager(0).
with_pager(Goal) :-
    setup_call_cleanup(
        with_pager_setup(PagerIn,Pid),
        with_output_to(PagerIn,Goal),
        with_pager_cleanup(PagerIn,Pid)
    ).


% set up a pager
with_pager_setup(PagerIn,Pid) :-
    once( getenv('PAGER',Pager)
        ; Pager=less
        ),
    process_create(
        path(Pager),
        [],
        [
            stdin(pipe(PagerIn)),
            stdout(std),
            process(Pid)
        ]
    ).

% clean up after a pager
with_pager_cleanup(PagerIn,Pid) :-
    close(PagerIn),
    process_wait(Pid,_Status).
