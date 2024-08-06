module Main exposing (..)

import Browser
import EnGrammar exposing (..)
import Grammar exposing (..)
import Html exposing (Html, button, div, footer, h1, header, main_, section, span, text)
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
    { sentence : Maybe SyntaxTree
    }


init : a -> ( Model, Cmd Msg )
init _ =
    ( { sentence = Nothing
      }
    , randomSentence en "Sentence"
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = Regenerate
    | Generated SyntaxTree


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Regenerate ->
            ( model, randomSentence en "Sentence" )

        Generated syntaxTree ->
            ( { sentence = Just syntaxTree }, Cmd.none )


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
                [ button [ onClick Regenerate ] [ text "Generate Sentence" ]
                , div [ style "padding-block" "1rem" ] [ maybeSyntaxTree model.sentence ]
                ]
            , section [ class "container", class "grammar-container" ]
                [ renderGrammar en ]
            ]
        , footer []
            [ text "© 2024" ]
        ]


maybeSyntaxTree : Maybe SyntaxTree -> Html Msg
maybeSyntaxTree x =
    case x of
        Nothing ->
            span [] [ text "<Press the button to generate a sentence>" ]

        Just tree ->
            syntaxTreeView tree
