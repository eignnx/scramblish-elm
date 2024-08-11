module Main exposing (..)

import Browser
import Dict
import EnGrammar exposing (..)
import Grammar exposing (..)
import Html exposing (Html, button, div, footer, h1, h3, header, hr, main_, section, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Logic
import Mutation exposing (GrammarMut, mutateSyntaxTree)
import Orthography exposing (chooseOrtho)
import Platform.Cmd as Cmd exposing (Cmd)
import Random
import Utils



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { examples : List SyntaxTree
    , scramblishGrammar : GrammarMut
    , querySolns : String
    }


init : a -> ( Model, Cmd Msg )
init _ =
    ( { examples = []
      , scramblishGrammar =
            { oldGrammar = en
            , newTitle = "Scramblish"
            , ruleMuts = []
            , wordMapping = Dict.empty
            , orthography = Orthography.romanOrthography
            }
      , querySolns = "<no solutions yet>"
      }
    , Cmd.batch
        [ generateScramblishGrammar
        , Utils.doCmd AddExample
        , Utils.doCmd AddExample
        , Utils.doCmd AddExample
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = AddExample
    | GeneratedExample SyntaxTree
    | MutateEnGrammar
    | MutationCreated Mutation.GrammarMut
    | RandomSolve
    | RandomSolution (List (Result Logic.SolveError Logic.USet))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddExample ->
            ( model, randomSentences en model.scramblishGrammar "Sentence" )

        GeneratedExample syntaxTree ->
            ( { model | examples = model.examples ++ [ syntaxTree ] }, Cmd.none )

        MutateEnGrammar ->
            ( model, generateScramblishGrammar )

        MutationCreated grammarMut ->
            ( { model | scramblishGrammar = grammarMut }, Cmd.none )

        RandomSolve ->
            ( model
            , Random.generate
                RandomSolution
                (Logic.randomSolveQuery
                    Logic.exDb
                    Logic.usetEmpty
                    [ Logic.Nt "mortal" [ Logic.Var "X" ] ]
                )
            )

        RandomSolution solns ->
            ( { model | querySolns = Debug.toString solns }, Cmd.none )


randomSentences : Grammar -> GrammarMut -> String -> Cmd Msg
randomSentences eng _ start =
    Random.generate
        GeneratedExample
        (generateSyntaxTree eng (Nt start))


generateScramblishGrammar : Cmd Msg
generateScramblishGrammar =
    Random.generate MutationCreated
        (chooseOrtho
            |> Random.andThen
                (\ortho ->
                    Mutation.grammarMutGenerator "Scramblish" ortho en
                )
        )



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "app-content" ]
        [ header []
            [ h1 []
                [ text "Scramblish" ]
            ]
        , main_ []
            [ section [ class "container" ]
                ((model.examples
                    |> List.indexedMap (sentenceExampleView model.scramblishGrammar)
                 )
                    ++ [ button [ onClick AddExample ] [ text "+ Additional Example" ] ]
                )
            , section [ class "container" ]
                [ button [ onClick RandomSolve ] [ text "Random Solve Query" ]
                , text model.querySolns
                ]

            -- [ Logic.solveQuery
            --     Logic.exDb
            --     Logic.usetEmpty
            --     [ Logic.Nt "mortal" [ Logic.Var "X" ] ]
            --     -- [ Logic.Nt "man" [ Logic.Atom "socrates" ] ]
            --     -- |> Debug.toString
            --     -- |> text
            --     |> List.map
            --         (\res ->
            --             case res of
            --                 Err e ->
            --                     e |> Debug.toString |> text
            --                 Ok us ->
            --                     us |> Logic.viewUSet
            --         )
            --     |> List.intersperse (hr [] [])
            --     |> div [ class "all-solutions" ]
            -- ]
            , section [ class "container", class "grammar-container" ]
                [ renderGrammar en ]
            , section [ class "container", class "grammar-container" ]
                [ button [ onClick MutateEnGrammar ] [ text "⟳ Regenerate Grammar Mutation" ]
                , renderGrammar <|
                    Mutation.applyGrammarMut model.scramblishGrammar
                ]
            ]
        , footer []
            [ text "© 2024" ]
        ]


sentenceExampleView : GrammarMut -> Int -> SyntaxTree -> Html msg
sentenceExampleView grammarMut index engTree =
    let
        scrTree =
            mutateSyntaxTree grammarMut engTree
    in
    div [ style "padding-block" "1rem", class "example" ]
        [ h3 [] [ text ("Example " ++ String.fromInt (index + 1)) ]
        , div [ class "translation" ]
            [ div []
                [ text "Scramblish:"
                , syntaxTreeView grammarMut.orthography.id scrTree
                ]
            , div []
                [ text "English:"
                , syntaxTreeView "english" engTree
                ]
            ]
        ]
