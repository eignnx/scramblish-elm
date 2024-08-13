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


db : T.Db
db =
    { rules =
        Dict.fromList
            [ ( "=", [ { params = [ T.Var "X", T.Var "X" ], body = [] } ] )
            , ( "append"
              , [ { params = [ T.Atom "[]", T.Var "Y", T.Var "Y" ], body = [] }
                , { params = [ T.Cons (T.Var "X") (T.Var "Xs"), T.Var "Ys", T.Cons (T.Var "X") (T.Var "Zs") ]
                  , body = [ T.Comp "append" [ T.Var "Xs", T.Var "Ys", T.Var "Zs" ] ]
                  }
                ]
              )
            , ( "-->"
              , [ { params = [ T.Comp "sentence" [ T.Var "Before", T.Var "After" ] ]
                  , body =
                        [ T.Comp "append"
                            [ T.Var "Before"
                            , T.toValList [ T.Text "the", T.Text "cat", T.Text "slept" ]
                            , T.Var "After"
                            ]
                        ]
                  }
                ]
              )
            , ( "phrase"
              , [ { params = [ T.Var "Rule", T.Var "Before", T.Var "After" ]
                  , body =
                        [ T.Comp "-->" [ T.Var "RuleHead" ]

                        -- TODO
                        ]
                  }
                ]
              )
            ]
    }
