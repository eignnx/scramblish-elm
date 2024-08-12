module Logic exposing (..)

{-| For logic programming.
-}

import Dict exposing (Dict)
import Html
import List.Extra
import Maybe.Extra
import Random
import Random.Extra
import Seq
import Utils


type Val
    = Text String
    | Var String
    | Atom String
    | Cons Val Val
    | Comp String Args


toValList : List Val -> Val
toValList list =
    case list of
        [] ->
            Atom "[]"

        x :: xs ->
            Cons x (toValList xs)


type alias Goal =
    Val


{-| A _conjunction_ of subgoals.
-}
type alias Query =
    List Goal


type alias Args =
    List Val


type alias Clause =
    { params : Args
    , body : Query
    }


{-| The logic engine database.
-}
type alias Db =
    { rules : Dict String (List Clause)
    }


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


{-| A set of unification facts.
-}
type alias USet =
    Dict String Val


usetEmpty : USet
usetEmpty =
    Dict.empty


unify : USet -> Val -> Val -> Maybe USet
unify u0 x y =
    case ( x, y ) of
        ( Text tx, Text ty ) ->
            if tx == ty then
                Just u0

            else
                Nothing

        ( Atom ax, Atom ay ) ->
            if ax == ay then
                Just u0

            else
                Nothing

        ( Comp nx px, Comp ny py ) ->
            if nx == ny then
                unifyList u0 px py

            else
                Nothing

        ( Cons h1 t1, Cons h2 t2 ) ->
            unify u0 h1 h2 |> Maybe.andThen (\u1 -> unify u1 t1 t2)

        ( Var vx, _ ) ->
            simplifyVal u0 y |> (\yy -> Dict.insert vx yy u0) |> Just

        ( _, Var vy ) ->
            simplifyVal u0 x |> (\xx -> Dict.insert vy xx u0) |> Just

        _ ->
            Nothing


usetLookup : USet -> String -> Maybe Val
usetLookup u v1 =
    Dict.get v1 u |> Maybe.andThen (mapVarMaybe (usetLookup u))


mapVarMaybe : (String -> Maybe Val) -> Val -> Maybe Val
mapVarMaybe f val =
    case val of
        Var v ->
            f v

        _ ->
            Just val


mapVar : (String -> Val) -> Val -> Val
mapVar f val =
    case val of
        Var v ->
            f v

        _ ->
            val



-- SOLVE QUERY


type SolveError
    = UncallableValue Val
    | UndefinedPredicate String
    | Failure
    | TypeError String Args


getClauses : Db -> String -> Result SolveError (List Clause)
getClauses db predName =
    Dict.get predName db.rules
        |> Result.fromMaybe (UndefinedPredicate predName)


findMatchingClauses : Db -> USet -> String -> Args -> Result SolveError (List ( USet, Query ))
findMatchingClauses db u0 predName argsDup =
    getClauses db predName
        |> Result.map
            (\clauses ->
                clauses
                    |> List.filterMap
                        (\clause ->
                            unifyList u0 argsDup clause.params
                                |> Maybe.map (\u1 -> ( u1, clause.body ))
                        )
            )


type alias SolnStream =
    Seq.Seq (Result SolveError USet)


solveGoal : Db -> USet -> Goal -> SolnStream
solveGoal db u0 goal =
    case goal of
        Comp predName args ->
            case findMatchingClauses db u0 predName (dupList args) of
                Err e ->
                    Seq.singleton (Err e)

                Ok clauseMatches ->
                    clauseMatches
                        |> Seq.fromList
                        |> Seq.flatMap (\( u1, body ) -> solveQuery db u1 body)

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


unifyList : USet -> List Val -> List Val -> Maybe USet
unifyList u0 xs ys =
    if List.length xs /= List.length ys then
        Nothing

    else
        List.Extra.zip xs ys
            |> Maybe.Extra.fold u0 (\u1 ( x, y ) -> unify u1 x y)


dupList : List Val -> List Val
dupList list =
    List.map dupVal list


dupVal : Val -> Val
dupVal val =
    val |> mapVar dupVar


dupVar : String -> Val
dupVar name =
    Var (name ++ "'")


simplifyVal : USet -> Val -> Val
simplifyVal u val =
    case val of
        Var v ->
            usetLookup u v |> Maybe.withDefault val

        Comp name args ->
            Comp name (args |> List.map (simplifyVal u))

        _ ->
            val


viewUSet : USet -> Html.Html a
viewUSet u =
    u
        |> Dict.foldl
            (\k v acc ->
                acc
                    ++ [ Html.tr []
                            [ Html.td [] [ Html.text k ]
                            , Html.td [] [ Html.text "->" ]
                            , Html.td []
                                [ v
                                    |> Debug.toString
                                    |> Html.text
                                ]
                            ]
                       ]
            )
            []
        |> (\rows -> Html.div [] rows)



-- RANDOM SOLVE QUERY


randomSolveGoal : Db -> USet -> Goal -> Random.Generator SolnStream
randomSolveGoal db u0 goal =
    case goal of
        Comp predName args ->
            case findMatchingClauses db u0 predName (dupList args) of
                Err e ->
                    Err e |> Seq.singleton |> Random.constant

                Ok clauseMatches ->
                    clauseMatches
                        |> Random.Extra.shuffled
                        |> Random.andThen (tryClauses db)

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



-- BUILTINS


unifying : USet -> Val -> Val -> SolnStream
unifying u x y =
    case unify u x y of
        Just u1 ->
            Seq.singleton (Ok u1)

        Nothing ->
            Seq.empty


builtins : Dict.Dict String (Db -> USet -> Args -> SolnStream)
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


fromValList : Val -> Maybe (List Val)
fromValList val =
    case val of
        Cons h t ->
            fromValList t |> Maybe.map (\ts -> h :: ts)

        Atom "[]" ->
            Just []

        _ ->
            Nothing
