:- module(amalog_lex, [tokens//1]).

:- use_module(library(amalog/dcg), [ at_least//2
                                   , at_least//3
                                   , black//0
                                   , black//1
                                   , eos//0
                                   , exactly//2
                                   , followed_by//1
                                   , generous//2
                                   , greedy//1
                                   , greedy//2
                                   , nl//0
                                   , space//1
                                   , white//1
                                   ]).

tokens(Ts) -->
    greedy(token,Ts),
    end,
    !.


token(T) -->
    word(T).
token(T) -->
    indent(T).
token(T) -->
    shelf(T).
token(T) -->
    text(T).
token(T) -->
    at_least(1,space),
    token(T).


word(Word) -->
    at_least(1,black,Word).


indent(indent(Level)) -->
    nl,
    greedy(space, Spaces),
    followed_by(black),
    { length(Spaces,N) },
    { 0 is N mod 4 },
    { Level is N div 4 }.


end -->
    nl,
    eos.
end -->
    greedy(white),
    eos.


shelf(shelf(Kind,Words)) -->
    left_bookend(Kind),
    words(Words),
    right_bookend(Kind).


text(text(Kind,Content)) -->
    quote(Kind),
    generous(char,Content),
    quote(Kind).


words([Word|Words]) -->
    word(Word),
    greedy(space_word,Words).


space_word(Word) -->
    at_least(1,space),
    word(Word).


left_bookend(Kind) -->
    [C],
    { left_bookend(Kind,C) }.


right_bookend(Kind) -->
    [C],
    { right_bookend(Kind,C) }.


left_bookend(round, 0'().
left_bookend(square,0'[).
left_bookend(curly, 0'{).


right_bookend(round, 0')).
right_bookend(square,0']).
right_bookend(curly, 0'}).


quote(single) -->
    [0''].
quote(double) -->
    [0'"]. %"
quote(back(N)) -->
    exactly(N,backtick),
    { N > 0 }.


backtick(0'`) -->
    [0'`].


% match any single character
char(C) -->
    [C].
