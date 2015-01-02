% rules describing single characters

nl -->
    "\n".


black -->
    black(_).

black(Char) -->
    [Char],
    { black_char(Char) }.


space -->
    space(_).

space(C) -->
    [C],
    { space_char(C) }.


white(Char) -->
    [Char],
    { white_char(Char) }.


black_char(C) :-
    \+ white_char(C),
    \+ quote_char(C),
    \+ bookend_char(C).


bookend_char(0'(). %'
bookend_char(0')). %'
bookend_char(0'[). %'
bookend_char(0']). %'
bookend_char(0'{). %'
bookend_char(0'}). %'


space_char(0'\s).


white_char(0'\s).
white_char(0'\n).


quote_char(0'').
quote_char(0'"). %"'
quote_char(0'`). %`'
