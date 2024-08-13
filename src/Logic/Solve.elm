module Logic.Solve exposing (..)

{-| For logic programming.
-}

import Dict exposing (Dict)
import Logic.Builtins
import Logic.Types exposing (..)
import Random
import Random.Extra
import Seq


exDb : Db
exDb =
    { rules =
        [ ( "mortal"
          , [ { params = [ Var "Entity" ], body = [ Comp "man" [ Var "Entity" ] ] }
            ]
          )
        , ( "man"
          , [ { params = [ Atom "socrates" ], body = [] }
            , { params = [ Atom "plato" ], body = [] }
            , { params = [ Atom "george" ], body = [] }
            , { params = [ Atom "frank" ], body = [] }
            , { params = [ Atom "ted" ], body = [] }
            ]
          )
        ]
            |> Dict.fromList
    }



-- SOLVE QUERY


solveGoal : Db -> USet -> Goal -> SolnStream
solveGoal db u0 goal =
    case simplifyVal u0 goal of
        Comp predName args ->
            let
                solveNonBuiltinGoal : () -> SolnStream
                solveNonBuiltinGoal () =
                    case findMatchingClauses db u0 predName (dupList args) of
                        Err e ->
                            Seq.singleton (Err e)

                        Ok clauseMatches ->
                            clauseMatches
                                |> Seq.fromList
                                |> Seq.flatMap (\( u1, body ) -> solveQuery db u1 body)

                solveBuiltinGoal : Logic.Types.BuiltinImpl -> SolnStream
                solveBuiltinGoal builtin =
                    builtin db u0 args
            in
            case Dict.get predName Logic.Builtins.builtins of
                Just builtin ->
                    solveBuiltinGoal builtin

                Nothing ->
                    solveNonBuiltinGoal ()

        _ ->
            Seq.singleton (Err (UncallableValue goal))


solveQuery : Db -> USet -> Query -> SolnStream
solveQuery db u0 queryParts =
    case queryParts of
        [] ->
            Seq.singleton (Ok u0)

        goal :: remainingGoals ->
            solveGoal db u0 goal
                |> Seq.flatMap
                    (\res ->
                        case res of
                            Err e ->
                                Seq.singleton (Err e)

                            Ok u1 ->
                                solveQuery db u1 remainingGoals
                    )


dupList : List Val -> List Val
dupList list =
    List.map dupVal list


dupVal : Val -> Val
dupVal val =
    val |> mapVar dupVar


dupVar : String -> Val
dupVar name =
    Var (name ++ "'")



-- RANDOM SOLVE QUERY


randomSolveGoal : Db -> USet -> Goal -> Random.Generator SolnStream
randomSolveGoal db u0 goal =
    case goal of
        Comp predName args ->
            let
                randomSolveGoalNotBuiltin : () -> Random.Generator SolnStream
                randomSolveGoalNotBuiltin () =
                    case findMatchingClauses db u0 predName (dupList args) of
                        Err e ->
                            Err e |> Seq.singleton |> Random.constant

                        Ok clauseMatches ->
                            clauseMatches
                                |> Random.Extra.shuffled
                                |> Random.andThen (tryClauses db)

                randomSolveGoalBuiltin : Logic.Types.BuiltinImpl -> Random.Generator SolnStream
                randomSolveGoalBuiltin builtin =
                    builtin db u0 args |> Random.constant
            in
            case Dict.get predName Logic.Builtins.builtins of
                Just builtin ->
                    randomSolveGoalBuiltin builtin

                Nothing ->
                    randomSolveGoalNotBuiltin ()

        _ ->
            Err (UncallableValue goal) |> Seq.singleton |> Random.constant


tryClauses : Db -> List ( USet, Query ) -> Random.Generator SolnStream
tryClauses db clauses =
    case clauses of
        [] ->
            Random.constant Seq.empty

        ( u, body ) :: remainingClauses ->
            Random.map2 Seq.append
                (randomSolveQuery db u body)
                (tryClauses db remainingClauses)


randomSolveQuery : Db -> USet -> Query -> Random.Generator SolnStream
randomSolveQuery db u0 queryParts =
    case queryParts of
        [] ->
            Ok u0 |> Seq.singleton |> Random.constant

        goal :: remainingGoals ->
            randomSolveGoal db u0 goal
                |> Random.andThen (randomSolveRemainingGoals db remainingGoals)


randomSolveRemainingGoals : Db -> Query -> SolnStream -> Random.Generator SolnStream
randomSolveRemainingGoals db remainingGoals solns =
    case Seq.next solns of
        Seq.Nil ->
            Random.constant Seq.empty

        Seq.Cons s ss ->
            case s of
                Err e ->
                    Err e |> Seq.singleton |> Random.constant

                Ok u ->
                    Random.map2 Seq.append
                        (randomSolveQuery db u remainingGoals)
                        (randomSolveRemainingGoals db remainingGoals ss)
