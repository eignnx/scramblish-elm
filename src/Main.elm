module Main exposing (..)

import Browser
import EnGrammar exposing (..)
import Grammar exposing (..)
import Html exposing (Html, button, div, footer, h1, h3, header, main_, section, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Mutation
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
    , scramblishGrammar : Grammar.Grammar
    }


init : a -> ( Model, Cmd Msg )
init _ =
    ( { examples = []
      , scramblishGrammar = { en | title = "Scramblish" }
      }
    , Cmd.none
      -- , Mutation.grammarMutGenerator "Scramblish" en
      --     |> Random.andThen (\scrGrammar ->
      --             randomSentences { eng = en, scr = scrGrammar } "Sentence"
      --         )
      --     |> Random.generate MutationCreated
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
            ( model, randomSentences { eng = en, scr = model.scramblishGrammar } "Sentence" )

        GeneratedExample syntaxTree ->
            ( { model | examples = syntaxTree :: model.examples }, Cmd.none )

        MutateEnGrammar ->
            ( model, generateScramblishGrammar )

        MutationCreated grammarMut ->
            ( { model | scramblishGrammar = Mutation.applyGrammarMut grammarMut }, Cmd.none )


randomSentences : Translation Grammar -> String -> Cmd Msg
randomSentences grammars start =
    Random.generate GeneratedExample
        (Random.pair
            (generateSyntaxTree grammars.eng (Nt start))
            (generateSyntaxTree grammars.scr (Nt start))
            |> Random.map
                (\( eng, scr ) -> { eng = eng, scr = scr })
        )


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
                    |> List.map exampleView
                    |> List.indexedMap sentenceExampleView
                 )
                    ++ [ button [ onClick AddExample ] [ text "+ Additional Example" ] ]
                )
            , section [ class "container", class "grammar-container" ]
                [ renderGrammar en ]
            , section [ class "container", class "grammar-container" ]
                [ button [ onClick MutateEnGrammar ] [ text "⟳ Regenerate Grammar Mutation" ]
                , renderGrammar model.scramblishGrammar
                ]
            ]
        , footer []
            [ text "© 2024" ]
        ]


sentenceExampleView : Int -> Html msg -> Html msg
sentenceExampleView index sentence =
    div [ style "padding-block" "1rem" ]
        [ h3 [] [ text ("Example " ++ String.fromInt (index + 1)) ]
        , sentence
        ]
