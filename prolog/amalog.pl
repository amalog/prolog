:- module(amalog, []).
:- use_module(library(sweet)).

:- use my(amalog/dcg).

:- use readutil -> read_stream_to_codes/2.

:- redefine_system_predicate(read(_,_)).

% predicates with a tail (foo_) are private. others can be accessed
% publicly and are declared that way for clarity.
:- public read/2.

read(codes(Codes),Term) :-
    !,
    read_(Codes,Term).
read(Source,Term) :-
    source_to_stream_(Source, Stream),
    cleanup(close(Stream)),
    read_stream_to_codes(Stream,Codes),
    read_(Codes,Term).


source_to_stream_(file(Path),Stream) :-
    open(Path,read,Stream).


read_(Codes,Term) :-
    once(phrase(term(T),Codes,Rest)),
    ( Term=T
    ; read_(Rest,Term)
    ).
