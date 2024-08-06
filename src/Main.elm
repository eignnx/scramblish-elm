module Main exposing (..)

import Browser
import EnGrammar exposing (..)
import Grammar exposing (..)
import Html exposing (Html, button, div, footer, h1, h3, header, main_, section, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Mutation exposing (GrammarMut, applyGrammarMut)
import Platform.Cmd as Cmd
import Random



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
    { examples : List (Translation SyntaxTree)
    , scramblishGrammar : GrammarMut
    }


init : a -> ( Model, Cmd Msg )
init _ =
    ( { examples = []
      , scramblishGrammar =
            { oldGrammar = en
            , newTitle = "Scramblish"
            , ruleMuts = []
            }
      }
    , Random.generate MutationCreated <| Mutation.grammarMutGenerator "Scramblish" en
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = AddExample
    | GeneratedExample (Translation SyntaxTree)
    | MutateEnGrammar
    | MutationCreated Mutation.GrammarMut


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


randomSentences : Grammar -> GrammarMut -> String -> Cmd Msg
randomSentences eng engMut start =
    Random.generate GeneratedExample
        (generateSyntaxTree eng (Nt start)
            |> Random.map (\engTree -> ( engTree, Mutation.mutateSyntaxTree engMut engTree ))
            |> Random.map
                (\( engTree, scrTree ) -> { eng = engTree, scr = scrTree })
        )


generateScramblishGrammar : Cmd Msg
generateScramblishGrammar =
    Random.generate MutationCreated (Mutation.grammarMutGenerator "Scramblish" en)



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
                    |> List.indexedMap sentenceExampleView
                 )
                    ++ [ button [ onClick AddExample ] [ text "+ Additional Example" ] ]
                )
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


sentenceExampleView : Int -> Translation SyntaxTree -> Html msg
sentenceExampleView index { eng, scr } =
    div [ style "padding-block" "1rem", class "example" ]
        [ h3 [] [ text ("Example " ++ String.fromInt (index + 1)) ]
        , div [ class "translation" ]
            [ div []
                [ text "Scramblish:"
                , syntaxTreeView scr
                ]
            , div []
                [ text "English:"
                , syntaxTreeView eng
                ]
            ]
        ]
