:- module(amalog, []).
:- use_module(library(sweet)).

:- use my(amalog/dcg).

:- use readutil -> read_stream_to_codes/2.

:- redefine_system_predicate(read(_,_)).

% predicates with a tail (foo_) are private. others can be accessed
% publicly and are declared that way for clarity.
:- public program/2.

program(codes(Codes),Program) :-
    !,
    program_(Codes,Program).
program(Source,Program) :-
    source_to_stream_(Source, Stream),
    cleanup(close(Stream)),
    read_stream_to_codes(Stream,Codes),
    program_(Codes,Program).

program_(Codes,Program) :-
    once(phrase(program(Program),Codes)).


source_to_stream_(file(Path),Stream) :-
    open(Path,read,Stream).
