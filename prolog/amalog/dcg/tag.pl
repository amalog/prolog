% describe the tag (aka: functor, name) of a term

:- multifile delay:mode/1.
delay:mode(amalog_dcg:tag_parts(list,_,_,_)).
delay:mode(amalog_dcg:tag_parts(_,ground,list,ground)).

%% tag(Tag:atom)//
tag(Tag) -->
    { delay(atom_codes(Tag,Codes)) },
    tag_(Codes).

tag_(Codes) -->
    { delay(tag_parts(Codes,First,Middle,Last)) },
    lowercase(First),
    greedy(one_of([lowercase,dash]), Middle),
    exactly(1, one_of([lowercase,tag_tail]), [Last]),
    !.
tag_([C]) -->
    lowercase(C).


% describe the three parts of a tag
tag_parts([First|Rest],First,Middle,Tail) :-
    append(Middle,[Tail],Rest).
