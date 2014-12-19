:- use_module(library(amalog/lex),[tokens//1]).

no_nl(0'\n,0'~). %' fix syntax highlighter
no_nl(X,X).

term_expansion(Text -> Tokens, (Head:-Body)) :-
    once(maplist(no_nl,Text,Clean)),
    format(atom(Head),"~s",[Clean]),
    Body = (
        phrase(tokens(Ts),Text),
        Ts == Tokens
    ),
    tap:register_test(Head).
    

:- use_module(library(tap)).

`hello world\n` -> [`hello`,`world`].

`hello\nstuff\nbye\n` -> [ `hello`
                         , indent(0)
                         , `stuff`
                         , indent(0)
                         , `bye`
                         ].

`hello _Whom\n    say hello\n` -> [ `hello`
                                  , `_Whom`
                                  , indent(1)
                                  , `say`
                                  , `hello`
                                  ].

`hello 'to everyone'\n` -> [ `hello`
                           , text(single,`to everyone`)
                           ].

`hello "to everyone"\n` -> [ `hello`
                           , text(double,`to everyone`)
                           ].

`hello ``to everyone``\n` -> [ `hello`
                             , text(back(1),`to everyone`)
                             ].

`hello ````to everyone````\n` -> [ `hello`
                                 , text(back(2),`to everyone`)
                                 ].

`perl ``use strict;\nsay "hello"\n``` ->
    [ `perl`
    , text(back(1),`use strict;\nsay "hello"\n`)
    ].

`alpha A\n\n\nbeta B\n` ->
    [ `alpha`
    , `A`
    , next_predicate
    , `beta`
    , `B`
    ].

`lang english\nlang french\nlang spanish\n\n\nland usa\nland canada\n` ->
    [ `lang`, `english`
    , indent(0)
    , `lang`, `french`
    , indent(0)
    , `lang`, `spanish`
    , next_predicate
    , `land`, `usa`
    , indent(0)
    , `land`, `canada`
    ].
