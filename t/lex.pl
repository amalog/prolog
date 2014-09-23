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
