:- use_module(library(amalog/lex),[tokens//1]).

:- use_module(library(tap)).

'single fact' :-
    phrase(tokens(Ts),`hello world\n`),
    Ts == [`hello`,`world`].

'three facts' :-
    phrase(tokens(Ts),`hello\nok\ngoodbye\n`),
    Ts == [ `hello`
          , indent(0)
          , `ok`
          , indent(0)
          , `goodbye`
          ].

'single clause predicate' :-
    phrase(tokens(Ts),`hello _Whom\n    say hello\n`),
    Ts == [ `hello`
          , `_Whom`
          , indent(1)
          , `say`
          , `hello`
          ].

'fact with single quoted text' :-
    phrase(tokens(Ts),`hello 'to everyone'\n`),
    Ts == [ `hello`
          , text(single,`to everyone`)
          ].

'fact with double quoted text' :-
    phrase(tokens(Ts),`hello "to everyone"\n`),
    Ts == [ `hello`
          , text(double,`to everyone`)
          ].

'fact with single backquoted text' :-
    phrase(tokens(Ts),`hello ``to everyone``\n`),
    Ts == [ `hello`
          , text(back(1),`to everyone`)
          ].

'fact with double backquoted text' :-
    phrase(tokens(Ts),`hello ````to everyone````\n`),
    Ts == [ `hello`
          , text(back(2),`to everyone`)
          ].

'fact with multiline backquoted text' :-
    phrase(tokens(Ts),`perl ``use strict;\nsay "hello"\n```),
    Ts == [ `perl`
          , text(back(1),`use strict;\nsay "hello"\n`)
          ].
