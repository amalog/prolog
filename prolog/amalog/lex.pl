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


%% tokens(Tokens:list)
%
%  True if Amalog text parses into Tokens.
tokens(Ts) -->
    greedy(token,Ts),
    end,
    !.


%% token(Token)
%
%  True if DCG list describes a valid Amalog token.
token(T) -->
    word(T).
token(T) -->
    indent(T).
token(next_predicate) -->
    next_predicate.
token(T) -->
    shelf(T).
token(T) -->
    text(T).
token(T) -->
    at_least(1,space),
    token(T).


% a non-empty string of black characters
word(Word) -->
    at_least(1,black,Word).


% describes the newline and space pattern indicative of indentation
% (indentation must be multiples of 4 spaces, no tabs)
indent(indent(Level)) -->
    nl,
    greedy(space, Spaces),
    followed_by(black),
    { length(Spaces,N) },
    { 0 is N mod 4 },
    { Level is N div 4 }.


%% describes the token which separates predicate definitions
next_predicate -->
    nl,
    nl,
    nl,
    followed_by(black).


% describes the end of an Amalog file
end -->
    nl,
    eos.
end -->
    greedy(white),
    eos.


% a shelf is a list of words bracketed by bookend characters like
% (), [] or {}.  Kind indicates which flavor of bookends was used.
shelf(shelf(Kind,Words)) -->
    left_bookend(Kind),
    words(Words),
    right_bookend(Kind).


% quoted textual content
text(text(Kind,Content)) -->
    quote(Kind),
    generous(char,Content),
    quote(Kind).


% describes a non-empty, space-separated list of Amalog words
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


left_bookend(round, 0'(). %'
left_bookend(square,0'[). %'
left_bookend(curly, 0'{). %'


right_bookend(round, 0')). %'
right_bookend(square,0']). %'
right_bookend(curly, 0'}). %'


quote(single) -->
    [0''].
quote(double) -->
    [0'"]. %"'
quote(back(N)) -->
    exactly(N,backtick),
    { N > 0 }.


backtick(0'`) --> %`'
    [0'`]. %`'


% match any single character
char(C) -->
    [C].
