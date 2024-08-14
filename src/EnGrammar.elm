module EnGrammar exposing (..)

import Dict
import Grammar exposing (Form(..), Grammar, Nt(..), Tm(..))
import Logic.Types as T


en : Grammar
en =
    { title = "English"
    , rules =
        [ ( Nt "Sentence", [ NtForm (Nt "NounPhrase"), NtForm (Nt "VerbPhrase") ] )
        , ( Nt "Sentence", [ NtForm (Nt "NounPhrase"), NtForm (Nt "PrepositionalPhrase"), NtForm (Nt "VerbPhrase") ] )
        , ( Nt "NounPhrase", [ NtForm (Nt "Determiner"), NtForm (Nt "AdjectivalNoun") ] )
        , ( Nt "Determiner", [ TmForm (Tm "the") ] )
        , ( Nt "Determiner", [ TmForm (Tm "a") ] )
        , ( Nt "Determiner", [ TmForm (Tm "every") ] )
        , ( Nt "AdjectivalNoun", [ NtForm (Nt "Adjective"), NtForm (Nt "Noun") ] )
        , ( Nt "AdjectivalNoun", [ NtForm (Nt "Noun") ] )
        , ( Nt "Adjective", [ TmForm (Tm "big") ] )
        , ( Nt "Adjective", [ TmForm (Tm "cute") ] )
        , ( Nt "Adjective", [ TmForm (Tm "red") ] )
        , ( Nt "Noun", [ TmForm (Tm "dog") ] )
        , ( Nt "Noun", [ TmForm (Tm "cat") ] )
        , ( Nt "Noun", [ TmForm (Tm "bird") ] )
        , ( Nt "VerbPhrase", [ NtForm (Nt "VerbIntransitive") ] )
        , ( Nt "VerbPhrase", [ NtForm (Nt "VerbTransitive"), NtForm (Nt "NounPhrase") ] )
        , ( Nt "VerbPhrase", [ NtForm (Nt "SemiCopula"), NtForm (Nt "Adjective") ] )
        , ( Nt "VerbPhrase", [ NtForm (Nt "SemiCopula"), NtForm (Nt "PrepositionalPhrase") ] )
        , ( Nt "VerbIntransitive", [ TmForm (Tm "sleeps") ] )
        , ( Nt "VerbTransitive", [ TmForm (Tm "sees") ] )
        , ( Nt "VerbTransitive", [ TmForm (Tm "likes") ] )
        , ( Nt "SemiCopula", [ TmForm (Tm "was") ] )
        , ( Nt "SemiCopula", [ TmForm (Tm "seemed") ] )
        , ( Nt "PrepositionalPhrase", [ NtForm (Nt "Preposition"), NtForm (Nt "NounPhrase") ] )
        , ( Nt "Preposition", [ TmForm (Tm "of") ] )
        , ( Nt "Preposition", [ TmForm (Tm "by") ] )
        , ( Nt "Preposition", [ TmForm (Tm "for") ] )
        ]
    }



-- LOGIC PROGRAM


enDb : T.Db
enDb =
    { rules =
        Dict.fromList
            [ ( "rule"
              , List.concat
                    [ -- rule(sentence, [noun_phrase(G,N,P), verb_phrase(G,N,P)]).
                      mkRule "sentence"
                        [ { params = []
                          , body =
                                [ T.Comp "noun_phrase" [ T.Var "G", T.Var "N", T.Var "P" ]
                                , T.Comp "verb_phrase" [ T.Var "G", T.Var "N", T.Var "P" ]
                                ]
                          }
                        ]
                    , mkRule "they"
                        [ -- rule(pronoun(Gender, sing, first), [['I']]).
                          { params = [ T.Var "_G", T.Atom "sing", T.Atom "first" ]
                          , body = [ atoms [ "I" ] ]
                          }

                        -- rule(pronoun(Gender, Number, second), [['you']]).
                        , { params = [ T.Var "_G", T.Var "Number", T.Atom "second" ]
                          , body = [ atoms [ "you" ] ]
                          }

                        -- rule(pronoun(masc, sing, third), [['he']]).
                        , { params = [ T.Atom "masc", T.Atom "sing", T.Atom "third" ]
                          , body = [ atoms [ "he" ] ]
                          }

                        -- rule(pronoun(femm, sing, third), [['she']]).
                        , { params = [ T.Atom "femm", T.Atom "sing", T.Atom "third" ]
                          , body = [ atoms [ "she" ] ]
                          }

                        -- rule(pronoun(nuet, sing, third), [['it']]).
                        , { params = [ T.Atom "nuet", T.Atom "sing", T.Atom "third" ]
                          , body = [ atoms [ "it" ] ]
                          }

                        -- rule(pronoun(enby, sing, third), [['they']]).
                        , { params = [ T.Atom "enby", T.Atom "sing", T.Atom "third" ]
                          , body = [ atoms [ "they" ] ]
                          }

                        -- rule(pronoun(Gender, plural, first), [['we']]).
                        , { params = [ T.Var "_G", T.Atom "plural", T.Atom "first" ]
                          , body = [ atoms [ "we" ] ]
                          }

                        -- rule(pronoun(Gender, plural, second), [['yall']]).
                        , { params = [ T.Var "_G", T.Atom "plural", T.Atom "second" ]
                          , body = [ atoms [ "y'all" ] ]
                          }

                        -- rule(pronoun(Gender, plural, second), [['you', 'all']]).
                        , { params = [ T.Var "_G", T.Atom "plural", T.Atom "second" ]
                          , body = [ atoms [ "you", "all" ] ]
                          }

                        -- rule(pronoun(Gender, plural, third), [['they']]).
                        , { params = [ T.Var "_G", T.Atom "plural", T.Atom "third" ]
                          , body = [ atoms [ "they" ] ]
                          }
                        ]
                    ]
              )
            ]
    }


type alias RuleClause =
    { params : T.Args
    , body : T.Query
    }


mkRule : String -> List RuleClause -> List T.Clause
mkRule ruleName argsQueries =
    argsQueries
        |> List.map
            (\{ params, body } ->
                { params = [ T.Comp ruleName params, T.toValList body ]
                , body = []
                }
            )


atoms : List String -> T.Val
atoms =
    List.map T.Atom >> T.toValList
