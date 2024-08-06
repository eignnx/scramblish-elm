module Main exposing (..)

import Browser
import EnGrammar exposing (..)
import Grammar exposing (..)
import Html exposing (Html, button, div, footer, h1, h3, header, main_, section, span, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
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
    { examples : List SyntaxTree
    }


init : a -> ( Model, Cmd Msg )
init _ =
    ( { examples = []
      }
    , randomSentence en "Sentence"
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = AddExample
    | Generated SyntaxTree


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddExample ->
            ( model, randomSentence en "Sentence" )

        Generated syntaxTree ->
            ( { model | examples = syntaxTree :: model.examples }, Cmd.none )


randomSentence : Grammar -> String -> Cmd Msg
randomSentence grammar start =
    Random.generate Generated (generateSyntaxTree grammar (Nt start))



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
                    |> List.map syntaxTreeView
                    |> List.indexedMap sentenceExampleView
                 )
                    ++ [ button [ onClick AddExample ] [ text "+ Additional Example" ] ]
                )
            , section [ class "container", class "grammar-container" ]
                [ renderGrammar en ]
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
