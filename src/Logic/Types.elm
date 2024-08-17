module Logic.Types exposing (..)

import Dict exposing (Dict)
import Html
import List.Extra
import Maybe.Extra
import Random
import Seq
import Utils


type Val
    = Text String
    | Var String
    | Atom String
    | Cons Val Val
    | Comp String Args


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


toValList : List Val -> Val
toValList list =
    case list of
        [] ->
            Atom "[]"

        x :: xs ->
            Cons x (toValList xs)


fromValList : Val -> Maybe (List Val)
fromValList val =
    case val of
        Cons h t ->
            fromValList t |> Maybe.map (\ts -> h :: ts)

        Atom "[]" ->
            Just []

        _ ->
            Nothing


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



-- DB


{-| The logic engine database.
-}
type alias Db =
    { rules : Dict String (List Clause)
    }


dbMerge : Db -> Db -> Db
dbMerge db1 db2 =
    { rules = Dict.union db1.rules db2.rules }


getClauses : Db -> String -> Result SolveError (List Clause)
getClauses db predName =
    Dict.get predName db.rules
        |> Result.fromMaybe (UndefinedPredicate predName)


findMatchingClauses : Db -> DupSubst -> USet -> String -> Args -> Result SolveError (List ( DupSubst, USet, Query ))
findMatchingClauses db dup0 u0 predName args =
    getClauses db predName
        |> Result.map
            (\clauses ->
                clauses
                    |> List.filterMap
                        (\clause ->
                            -- Duplicate the clause's parameters and body so
                            -- that the variables in the source code are
                            -- never accessable to other parts of the Prolog
                            -- program.
                            let
                                ( dup1, clauseParamsDuped ) =
                                    dupList dup0 clause.params

                                ( dup2, clauseBodyDuped ) =
                                    dupList dup1 clause.body
                            in
                            unifyList u0 args clauseParamsDuped
                                |> Maybe.map (\u1 -> ( dup2, u1, clauseBodyDuped ))
                        )
            )



-- USET


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


unifyList : USet -> List Val -> List Val -> Maybe USet
unifyList u0 xs ys =
    if List.length xs /= List.length ys then
        Nothing

    else
        List.Extra.zip xs ys
            |> Maybe.Extra.fold u0 (\u1 ( x, y ) -> unify u1 x y)


usetLookup : USet -> String -> Maybe Val
usetLookup u v1 =
    Dict.get v1 u |> Maybe.andThen (mapVarMaybe (usetLookup u))


simplifyVal : USet -> Val -> Val
simplifyVal u val =
    case val of
        Var v ->
            usetLookup u v |> Maybe.withDefault val

        Comp name args ->
            Comp name (args |> List.map (simplifyVal u))

        _ ->
            val


simplifyUSet : USet -> USet
simplifyUSet u =
    u |> Dict.map (\_ v -> simplifyVal u v)


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



-- DUP SUBST


{-| Variable-duplication substitution map.
-}
type alias DupSubst =
    { mapping : Dict.Dict String String
    , counter : Int
    }


emptyDupSubst : DupSubst
emptyDupSubst =
    { mapping = Dict.empty
    , counter = 2
    }


dupList : DupSubst -> List Val -> ( DupSubst, List Val )
dupList dup list =
    case list of
        [] ->
            ( dup, [] )

        x :: xs ->
            let
                ( dup1, x1 ) =
                    dupVal dup x

                ( dup2, xs2 ) =
                    dupList dup1 xs
            in
            ( dup2, x1 :: xs2 )


dupVal : DupSubst -> Val -> ( DupSubst, Val )
dupVal dup val =
    case val of
        Atom _ ->
            ( dup, val )

        Text _ ->
            ( dup, val )

        Var name ->
            dupVar dup name

        Cons h t ->
            let
                ( dup1, h1 ) =
                    dupVal dup h

                ( dup2, t2 ) =
                    dupVal dup1 t
            in
            ( dup2, Cons h1 t2 )

        Comp name args ->
            let
                ( dup1, args1 ) =
                    dupList dup args
            in
            ( dup1, Comp name args1 )


dupVar : DupSubst -> String -> ( DupSubst, Val )
dupVar dup name =
    case Dict.get name dup.mapping of
        Just name1 ->
            ( dup, Var name1 )

        Nothing ->
            let
                name1 =
                    name ++ "#" ++ String.fromInt dup.counter

                dup1 =
                    { dup
                        | counter = dup.counter + 1
                        , mapping = Dict.insert name name1 dup.mapping
                    }
            in
            ( dup1, Var name1 )



-- SOLVE ERROR


type SolveError
    = UncallableValue Val
    | UndefinedPredicate String
    | Failure
    | TypeError String Args



-- SOLUTION STREAM


type alias SolnStream =
    Seq.Seq (Result SolveError ( DupSubst, USet ))


unifying : DupSubst -> USet -> Val -> Val -> SolnStream
unifying dup u x y =
    case unify u x y of
        Just u1 ->
            Seq.singleton (Ok ( dup, u1 ))

        Nothing ->
            Seq.empty



-- BUILTIN IMPL


type alias BuiltinImpl =
    Db -> DupSubst -> USet -> Args -> SolnStream


type alias BuiltinImplRandom =
    Db -> DupSubst -> USet -> Args -> Random.Generator SolnStream
