% describe a key which names a property of a term

:- multifile delay:mode/1.
delay:mode(amalog_dcg:key_parts(list,_,_)).
delay:mode(amalog_dcg:key_parts(_,ground,list)).

%% key(Key:atom)//
key(Key) -->
    { delay(atom_codes(Key,Codes)) },
    key_(Codes).

key_(Codes) -->
    { delay(key_parts(Codes,First,Middle)) },
    lowercase(First),
    greedy(one_of([lowercase,dash]), Middle),
    ":",
    !.
key_([C, 0':]) --> %'
    lowercase(C),
    ":".


% describe the three parts of a key
key_parts([First|Rest],First,Middle) :-
    append(Middle,`:`,Rest).
