
rule(pronoun(Gender, sing, first), [['I']]).
rule(pronoun(Gender, Number, second), [['you']]).
rule(pronoun(masc, sing, third), [['he']]).
rule(pronoun(femm, sing, third), [['she']]).
rule(pronoun(nuet, sing, third), [['it']]).
rule(pronoun(enby, sing, third), [['they']]).
rule(pronoun(Gender, plural, first), [['we']]).
rule(pronoun(Gender, plural, second), [['yall']]).
rule(pronoun(Gender, plural, second), [['you', 'all']]).
rule(pronoun(Gender, plural, third), [['they']]).

rule(noun_phrase(G,N,P), [ pronoun(G,N,P) ]).
rule(noun_phrase(neut, N, third), [ det(N), noun(N) ]).
rule(det(_), [['the']]).
rule(det(sing), [['a']]).
rule(det(plural), [['some']]).
rule(noun(sing), [['cat']]).
rule(noun(plural), [['dogs']]).
rule(verb_phrase(G, N, P), [ verb(G, N, P), noun_phrase(_G2, _N2, _P2) ]).
rule(sentence, [noun_phrase(G,N,P), verb_phrase(G,N,P)]).

rule(verb(_G,   plural, _P),     [['see']]).
rule(verb(_G,   sing,   second), [['see']])
rule(verb(_G,   sing,   first),  [['see']]).
rule(verb(enby, sing,   third),  [['see']]).
rule(verb(masc, sing,   third),  [['sees']]).
rule(verb(femm, sing,   third),  [['sees']]).
rule(verb(neut, sing,   third),  [['sees']]).

my_phrase(Rule, After) :-
    my_phrase(Rule, [], After).

my_phrase(Rule, Before, After) :-
    rule(Rule, Body),
    interpret_body_conjunction(Body, Before, After).

interpret_body_conjunction([], Before, Before).
interpret_body_conjunction([SubRule|SubRules], Before, After) :-
    interpret_body(SubRule, Before, Middle),
    interpret_body_conjunction(SubRules, Middle, After).

interpret_body([], Before, Before).
interpret_body([X|Xs], Before, After) :-
    append(Before, [X|Xs], After).
interpret_body(NonTerminal, Before, After) :-
    my_phrase(NonTerminal, Before, After).

