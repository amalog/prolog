backtick(0'`) --> % ' syntax highlighter
    "`".

binary(B) -->
    { delay(string_codes(B,Codes)) },
    { delay(backtick_count(Codes,InternalTickCount)) },
    { delay(succ(InternalTickCount,ExternalTickCount)) },
    backticks(ExternalTickCount),
    generous(any,Codes),
    backticks(ExternalTickCount).

any(C) --> [C].

backticks(N) -->
    exactly(N,backtick).

backtick_count(Codes, N) :-
    foldl(count_backtick, Codes, 0, N).

count_backtick(X,N0,N) :-
    ( X = 0'` -> Incr=1; Incr=0 ), % ' syntax
    plus(N0,Incr,N).
