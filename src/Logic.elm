module Logic exposing (..)

{-| For logic programming.
-}

import Dict exposing (Dict)
import List.Extra
import Utils


type Val
    = Text String
    | Var String
    | Atom String
    | Nt String (List Val)


type alias Clause =
    { params : List Val
    , body : List Val
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


{-| Solves the query for the first solution only.
-}
findFirstSoln : Db -> USet -> List Val -> Result SolveError USet
findFirstSoln db u0 queryParts =
    case queryParts of
        [] ->
            Ok u0

        -- Success
        q :: qs ->
            case q of
                Nt predName args ->
                    let
                        argsDup =
                            dupList args
                    in
                    Dict.get predName db.rules
                        |> Result.fromMaybe (UndefinedPredicate predName)
                        |> Result.andThen
                            (\clauses ->
                                clauses
                                    |> List.Extra.findMap
                                        (\clause ->
                                            unifyList u0 argsDup clause.params
                                                |> Maybe.map (\u1 -> ( u1, clause.body ))
                                        )
                                    |> Result.fromMaybe Failure
                                    |> Result.andThen (\( u1, body ) -> findFirstSoln db u1 body)
                            )

                _ ->
                    Err (UncallableValue q)


unifyList : USet -> List Val -> List Val -> Maybe USet
unifyList u0 xs ys =
    if List.length xs /= List.length ys then
        Nothing

    else
        List.Extra.zip xs ys
            |> Utils.foldMaybe u0 (\u1 ( x, y ) -> unify u1 x y)


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
