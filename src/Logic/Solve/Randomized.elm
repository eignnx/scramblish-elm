module Logic.Solve.Randomized exposing (..)

import Dict
import Logic.Builtins exposing (..)
import Logic.Types exposing (..)
import Random
import Random.Extra
import Seq


tryClauses : Db -> List ( DupSubst, USet, Query ) -> Random.Generator SolnStream
tryClauses db clauses =
    case clauses of
        [] ->
            Random.constant Seq.empty

        ( dup, u, body ) :: remainingClauses ->
            -- Ok to use the same `dup` for both because this is a disjunction
            -- of solutions (i.e. the query will be solved by clause `body`,
            -- OR by one of the remaining clauses).
            Random.map2
                Seq.append
                (solveQuery db dup u body)
                (tryClauses db remainingClauses)



{-
   ```rust
   fn solve_query(u: USet, dup: DupSubst, query: Vec<Val>) -> impl SolnStream {
       let init: Box<dyn SolnStream> = soln_stream::success(u);

        query.cloning_iter().fold(init, |solns, goal| {
            Box::new(solns.flat_map(move |u_res| match u_res {
                Ok(u) => self.solve_query_impl(goal.clone(), u, dup),
                Err(e) => e.into(),
            }))
        })
   }
   ```
-}


solveQuery : Db -> DupSubst -> USet -> Query -> Random.Generator SolnStream
solveQuery db dup0 u0 queryParts =
    let
        init =
            Ok ( dup0, u0 ) |> Seq.singleton

        trySolveGoal : Goal -> Result SolveError ( DupSubst, USet ) -> Random.Generator SolnStream
        trySolveGoal goal res =
            case res of
                Err e ->
                    Err e |> Seq.singleton |> Random.constant

                Ok ( dup1, u1 ) ->
                    solveGoal db dup1 u1 goal
    in
    queryParts
        |> Seq.fromList
        |> randomSeqFoldl
            (\solns goal -> solns |> randomSeqFlatMap (trySolveGoal goal))
            init


randomSeqFoldl : (acc -> ele -> Random.Generator acc) -> acc -> Seq.Seq ele -> Random.Generator acc
randomSeqFoldl f acc seq =
    case Seq.next seq of
        Seq.Nil ->
            Random.constant acc

        Seq.Cons x xs ->
            f acc x |> Random.andThen (\y -> randomSeqFoldl f y xs)


randomSeqFlatMap : (x -> Random.Generator (Seq.Seq y)) -> Seq.Seq x -> Random.Generator (Seq.Seq y)
randomSeqFlatMap f seq =
    case Seq.next seq of
        Seq.Nil ->
            Random.constant Seq.empty

        Seq.Cons x xs ->
            Random.map2
                Seq.append
                (f x)
                (randomSeqFlatMap f xs)


solveGoal : Db -> DupSubst -> USet -> Goal -> Random.Generator SolnStream
solveGoal db dup0 u0 goal =
    case goal of
        Comp predName args ->
            let
                ( dup1, argsDuped ) =
                    dupList dup0 args

                ------ TODO:
                -- * Duplicate the CLAUSE and CLAUSE HEAD, not the ARGS.
                -- * Unify the ARGS with the duplicated CLAUSE HEAD ARGS.
                solveGoalNotBuiltin : () -> Random.Generator SolnStream
                solveGoalNotBuiltin () =
                    case findMatchingClauses db dup1 u0 predName argsDuped of
                        Err e ->
                            Err e |> Seq.singleton |> Random.constant

                        Ok clauseMatches ->
                            clauseMatches
                                |> Random.Extra.shuffled
                                |> Random.andThen (\list -> tryClauses db list)

                solveGoalBuiltin : Logic.Types.BuiltinImplRandom -> Random.Generator SolnStream
                solveGoalBuiltin builtin =
                    builtin db dup1 u0 argsDuped
            in
            case Dict.get predName Logic.Builtins.builtinsRandom of
                Just builtin ->
                    solveGoalBuiltin builtin

                Nothing ->
                    solveGoalNotBuiltin ()

        _ ->
            Err (UncallableValue goal) |> Seq.singleton |> Random.constant
