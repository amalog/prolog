% describe a term

term(Level,Term) -->
    multiline_term(Level,Term).
term(Level,Term) -->
    uniline_term(Level,Term).


multiline_term(IndentLevel0,Term) -->
    tag(Name),
    { succ(IndentLevel0, IndentLevel) },
    term_separator(IndentLevel),
    list(term(IndentLevel), term_separator(IndentLevel), Args),
    { list_dict(Name,Args,Term) }.


uniline_term(_IndentLevel,Term) -->
    { delay(list_dict(Name,List,Term)) },
    tag(Name),
    ( word_separator,
      list(uniline_argument, word_separator, List)
    ; { List = [] }
    ).


word_separator -->
    space.


term_separator(IndentLevel) -->
    nl,
    indent(IndentLevel).
