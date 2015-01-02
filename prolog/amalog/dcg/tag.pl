% describe the tag (aka: functor, name) of a term

%% tag(Word:atom)//
tag(Word) -->
    { delay(atom_codes(Word,Codes)) },
    at_least(1,black,Codes),
    !.
