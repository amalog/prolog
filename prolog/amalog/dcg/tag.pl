% describe the tag (aka: functor, name) of a term

:- multifile delay:mode/1.
delay:mode(amalog_dcg:split_tag(list,_,_)).
delay:mode(amalog_dcg:split_tag(_,_,list)). % approximately

% tag(Tag:atom)//
tag(Tag) -->
    { delay(atom_codes(Tag,Codes)) },
    { delay(split_tag(Codes,Words,Tail)) },
    split_tag(Words,Tail).


% split_tag(Tag:codes,Words:list(codes),Tail:list(codes))
split_tag(Tag,Words,Tail) :-
    phrase(split_tag(Words,Tail),Tag).

split_tag(Words,Tail) -->
    list(lowercase_word,dash,Words),
    at_most(1,tag_tail,Tail).
