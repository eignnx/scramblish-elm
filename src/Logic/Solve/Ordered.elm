module Logic.Solve.Ordered exposing (..)

import Dict
import Logic.Builtins exposing (..)
import Logic.Types exposing (..)
import Seq


solveGoal : Db -> DupSubst -> USet -> Goal -> SolnStream
solveGoal db dup0 u0 goal =
    case simplifyVal u0 goal of
        Comp predName args ->
            let
                ( dup1, args1 ) =
                    dupList dup0 args

                solveNonBuiltinGoal : () -> SolnStream
                solveNonBuiltinGoal () =
                    case findMatchingClauses db dup1 u0 predName args1 of
                        Err e ->
                            Seq.singleton (Err e)

                        Ok clauseMatches ->
                            clauseMatches
                                |> Seq.fromList
                                |> Seq.flatMap (\( dup2, u1, body ) -> solveQuery db dup2 u1 body)

                solveBuiltinGoal : Logic.Types.BuiltinImpl -> SolnStream
                solveBuiltinGoal builtin =
                    builtin db dup1 u0 args
            in
            case Dict.get predName Logic.Builtins.builtins of
                Just builtin ->
                    solveBuiltinGoal builtin

                Nothing ->
                    solveNonBuiltinGoal ()

        _ ->
            Seq.singleton (Err (UncallableValue goal))


solveQuery : Db -> DupSubst -> USet -> Query -> SolnStream
solveQuery db dup0 u0 queryParts =
    case queryParts of
        [] ->
            Seq.singleton (Ok ( dup0, u0 ))

        goal :: remainingGoals ->
            solveGoal db dup0 u0 goal
                |> Seq.flatMap
                    (\res ->
                        case res of
                            Err e ->
                                Seq.singleton (Err e)

                            Ok ( dup1, u1 ) ->
                                solveQuery db dup1 u1 remainingGoals
                    )
