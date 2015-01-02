% generic utilities that might end up in dcg_util someday

% remember the starting point of a DCG capture
capture_start(H,H,H).

% grab content since the starting point of the DCG capture
capture_end(End,[],End,End).
capture_end([Code|Rest],[Code|Codes],End,End) :-
    capture_end(Rest,Codes,End,End).


% alternation among several DCG rules
one_of([Rule|_],Match) -->
    call(Rule,Match).
one_of([_|Rules],Match) -->
    one_of(Rules,Match).


%% at_most(N:nonneg,:Dcg,Matches:list)//
%
%  Greedily consumes at most N matches of Dcg.  Dcg is called
%  with one extra parameter which should be bound to a representation
%  of what Dcg parsed.  Gives back matches on backtracking.
:- meta_predicate at_most(+,3,?,*,*).
at_most(N0,Rule,[X|Xs]) -->
    { N0 > 0 },
    call(Rule,X),
    { N is N0 - 1 },
    at_most(N,Rule,Xs).
at_most(_,_,[]) -->
    [].
