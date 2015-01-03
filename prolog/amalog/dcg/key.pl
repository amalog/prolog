% describe a key which names a property of a term

:- multifile delay:mode/1.
delay:mode(amalog_dcg:split_key(list,_)).
delay:mode(amalog_dcg:split_key(_,list)).

% key(Key:atom)//
key(Key) -->
    { delay(atom_codes(Key,Codes)) },
    { delay(split_key(Codes,Words)) },
    split_key(Words).


% split_key(Key:codes,Words:list(codes))
split_key(Key,Words) :-
    phrase(split_key(Words),Key).

split_key(Words) -->
    list(lowercase_word,dash,Words),
    ":".
