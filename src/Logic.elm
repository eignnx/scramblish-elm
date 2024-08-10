module Logic exposing (..)

{-| For logic programming.
-}

import Dict exposing (Dict)
import List.Extra
import Maybe.Extra
import Utils


type Val
    = Text String
    | Var String
    | Atom String
    | Nt String Args


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
          , [ { params = [ Var "Entity" ], body = [ Nt "man" [ Var "Entity" ] ] }
            ]
          )
        , ( "man"
          , [ { params = [ Atom "socrates" ], body = [] } ]
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

        ( Nt nx px, Nt ny py ) ->
            if nx == ny then
                unifyList u0 px py

            else
                Nothing

        ( Var vx, _ ) ->
            simplifyVal u0 y |> (\yy -> Dict.insert vx yy u0) |> Just

        ( _, Var vy ) ->
            simplifyVal u0 x |> (\xx -> Dict.insert vy xx u0) |> Just

        ( Text _, Atom _ ) ->
            Nothing

        ( Text _, Nt _ _ ) ->
            Nothing

        ( Atom _, Text _ ) ->
            Nothing

        ( Atom _, Nt _ _ ) ->
            Nothing

        ( Nt _ _, Text _ ) ->
            Nothing

        ( Nt _ _, Atom _ ) ->
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


getClauses : Db -> String -> Result SolveError (List Clause)
getClauses db predName =
    Dict.get predName db.rules
        |> Result.fromMaybe (UndefinedPredicate predName)


findMatchingClause : USet -> Args -> List Clause -> Result SolveError ( USet, List Val )
findMatchingClause u0 args clauses =
    clauses
        |> List.Extra.findMap
            (\clause ->
                unifyList u0 args clause.params
                    |> Maybe.map (\u1 -> ( u1, clause.body ))
            )
        |> Result.fromMaybe Failure


findFirstClause : Db -> USet -> String -> Args -> Result SolveError ( USet, List Val )
findFirstClause db u0 predName argsDup =
    getClauses db predName
        |> Result.andThen (findMatchingClause u0 argsDup)


findFirstSolnFromVal : Db -> USet -> Goal -> Result SolveError USet
findFirstSolnFromVal db u0 goal =
    case goal of
        Nt predName args ->
            findFirstClause db u0 predName (dupList args)
                |> Result.andThen (\( u1, body ) -> findFirstSoln db u1 body)

        _ ->
            Err (UncallableValue goal)


{-| Solves the query for the first solution only.
-}
findFirstSoln : Db -> USet -> Query -> Result SolveError USet
findFirstSoln db u0 queryParts =
    case queryParts of
        [] ->
            Ok u0

        goal :: remainingGoals ->
            findFirstSolnFromVal db u0 goal
                |> Result.andThen (\u2 -> findFirstSoln db u2 remainingGoals)


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

        Nt name args ->
            Nt name (args |> List.map (simplifyVal u))

        _ ->
            val
