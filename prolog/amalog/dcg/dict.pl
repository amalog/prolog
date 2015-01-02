:- multifile delay:mode/1.
delay:mode(amalog_dcg:dict_pairs(nonvar,_,_)).
delay:mode(amalog_dcg:dict_pairs(_,_,list)).

delay:mode(amalog_dcg:list_dict(_,list,_)).
delay:mode(amalog_dcg:list_dict(_,_,nonvar)).


% alias for system:dict_pairs/3 which fails (instead of throwing
% an exception when the dict is not a dict).
:- redefine_system_predicate(dict_pairs(_,_,_)).
dict_pairs(Dict,Tag,Pairs) :-
    once(var(Dict); is_dict(Dict)),
    system:dict_pairs(Dict,Tag,Pairs).


list_dict(Name, Values, Dict) :-
    delay(dict_pairs(Dict,Name,Pairs)),
    once(phrase(list_dict_(1,Pairs),Values)).


list_dict_(N,[Key-Value|Pairs]) -->
    [K,Value],
    { is_key(K,Key) },
    list_dict_(N,Pairs).
list_dict_(N0,[N0-Value|Pairs]) -->
    [Value],
    { succ(N0,N) },
    list_dict_(N,Pairs).
list_dict_(_,[]) --> [].
