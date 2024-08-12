module EnGrammar exposing (..)

import Dict
import Grammar exposing (Form(..), Grammar, Nt(..), Tm(..))
import Logic as L


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


db : L.Db
db =
    { rules =
        Dict.fromList
            [ ( "=", [ { params = [ L.Var "X", L.Var "X" ], body = [] } ] )
            , ( "append"
              , [ { params = [ L.Atom "[]", L.Var "Y", L.Var "Y" ], body = [] }
                , { params = [ L.Cons (L.Var "X") (L.Var "Xs"), L.Var "Ys", L.Cons (L.Var "X") (L.Var "Zs") ]
                  , body = [ L.Comp "append" [ L.Var "Xs", L.Var "Ys", L.Var "Zs" ] ]
                  }
                ]
              )
            , ( "-->"
              , [ { params = [ L.Comp "sentence" [ L.Var "Before", L.Var "After" ] ]
                  , body =
                        [ L.Comp "append"
                            [ L.Var "Before"
                            , L.toValList [ L.Text "the", L.Text "cat", L.Text "slept" ]
                            , L.Var "After"
                            ]
                        ]
                  }
                ]
              )
            , ( "phrase"
              , [ { params = [ L.Var "Rule", L.Var "Before", L.Var "After" ]
                  , body =
                        [ L.Comp "-->" [ L.Var "RuleHead" ]

                        -- TODO
                        ]
                  }
                ]
              )
            ]
    }
