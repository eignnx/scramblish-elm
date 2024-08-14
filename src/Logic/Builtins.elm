module Logic.Builtins exposing (..)

import Dict
import Logic.Types exposing (..)
import Seq


builtins : Dict.Dict String BuiltinImpl
builtins =
    Dict.fromList
        [ ( "clause_head_clause_body"
          , \db u0 params ->
                case params of
                    [ Comp predName args, body ] ->
                        case findMatchingClauses db u0 predName args of
                            Err e ->
                                Seq.singleton (Err e)

                            Ok clauseMatches ->
                                clauseMatches
                                    |> Seq.fromList
                                    |> Seq.flatMap
                                        (\( u1, clauseBody ) ->
                                            unifying u1 body (toValList clauseBody)
                                        )

                    [ Var v, body ] ->
                        db.rules
                            |> Dict.toList
                            |> List.concatMap
                                (\( predName, clauses ) ->
                                    clauses |> List.map (\clause -> ( predName, clause.params, clause.body ))
                                )
                            |> Seq.fromList
                            |> Seq.flatMap
                                (\( predName, params2, clauseBody ) ->
                                    unifying u0 (Comp predName params2) (Var v)
                                        |> Seq.flatMap
                                            (\res ->
                                                case res of
                                                    Err e ->
                                                        Seq.singleton (Err e)

                                                    Ok u1 ->
                                                        unifying u1 body (toValList clauseBody)
                                            )
                                )

                    _ ->
                        Seq.singleton (Err (TypeError "Invalid arguments to clause_head_clause_body" params))
          )

        -- , ( "=.."
        --   , \db u0 params ->
        --         case params of
        --             -- `V =.. [name, Arg1, Arg2]`
        --             [ Var v, Cons name args ] ->
        --                 let
        --                     nameStrRes =
        --                         case name of
        --                             Atom str ->
        --                                 Ok str
        --                             _ ->
        --                                 Err (TypeError "Cannot construct a compound term with a non-atom name." params)
        --                     argsAsListRes : Result SolveError (List Val)
        --                     argsAsListRes =
        --                         fromValList args |> Result.fromMaybe (TypeError "Non-list tail on RHS of =.." params)
        --                 in
        --                 case ( nameStrRes, argsAsListRes ) of
        --                     ( Ok nameStr, Ok argsAsList ) ->
        --                         unify u0 (Comp nameStr argsAsList) (Var v)
        --                             |> Maybe.map (\u1 -> Seq.singleton (Ok u1))
        --                             |> Maybe.withDefault Seq.empty
        --                     _ ->
        --                         Seq.singleton (Err (TypeError "Invalid arguments to =.." params))
        --             -- `foo(Arg1, Arg2) =.. Whatever`
        --             [ Comp name args, other ] ->
        --                 unifying u0 other (toValList (Atom name :: args))
        --             _ ->
        --                 Seq.singleton (Err (TypeError "Invalid arguments to =.." params))
        --   )
        ]



{-
   phrase(Rule, After) :-
       phrase(Rule, [], After).

   phrase(Rule, Before, After) :-
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
       phrase(NonTerminal, Before, After).
-}


stdDb : Db
stdDb =
    { rules =
        Dict.fromList
            [ ( "phrase"
              , [ { params = [ Var "Rule", Var "After" ]
                  , body = [ Comp "phrase" [ Var "Rule", Atom "[]" ], Var "After" ]
                  }
                , { params = [ Var "Rule", Var "Before", Var "After" ]
                  , body =
                        [ Comp "rule" [ Var "Rule", Var "Body" ]
                        , Comp "interpret_body_conjunction" [ Var "Body", Var "Before", Var "After" ]
                        ]
                  }
                ]
              )
            , ( "interpret_body_conjunction"
              , [ { params = [ Atom "[]", Var "Before", Var "Before" ], body = [] }
                , { params = [ Cons (Var "SubRule") (Var "SubRules"), Var "Before", Var "After" ]
                  , body =
                        [ Comp "interpret_body" [ Var "SubRule", Var "Before", Var "Middle" ]
                        , Comp "interpret_body_conjunction" [ Var "SubRules", Var "Middle", Var "After" ]
                        ]
                  }
                ]
              )
            , ( "interpret_body"
              , [ { params = [ Atom "[]", Var "Before", Var "Before" ], body = [] }
                , { params = [ Cons (Var "X") (Var "Xs"), Var "Before", Var "After" ]
                  , body = [ Comp "append" [ Var "Before", Cons (Var "X") (Var "Xs"), Var "After" ] ]
                  }
                , { params = [ Var "NonTerminal", Var "Before", Var "After" ]
                  , body = [ Comp "phrase" [ Var "NonTerminal", Var "Before", Var "After" ] ]
                  }
                ]
              )
            , ( "=", [ { params = [ Var "X", Var "X" ], body = [] } ] )
            , ( "append"
              , [ { params = [ Atom "[]", Var "Y", Var "Y" ], body = [] }
                , { params = [ Cons (Var "X") (Var "Xs"), Var "Ys", Cons (Var "X") (Var "Zs") ]
                  , body = [ Comp "append" [ Var "Xs", Var "Ys", Var "Zs" ] ]
                  }
                ]
              )
            ]
    }
