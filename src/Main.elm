module Main exposing (..)

import Browser
import Dict
import EnGrammar exposing (..)
import Grammar exposing (..)
import Html exposing (Html, button, dd, details, div, dl, dt, footer, h1, h3, header, main_, p, section, span, summary, text)
import Html.Attributes exposing (attribute, class, id, style)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Logic.Builtins
import Logic.Solve.Randomized
import Logic.Types as T
import Mutation exposing (GrammarMut, mutateSyntaxTree)
import Platform.Cmd as Cmd exposing (Cmd)
import Random
import Seq
import Set
import Utils
import WordGen.Gen as WordGen
import WordGen.Ortho
import WordGen.Phonology as Syllable
import WordStats exposing (WordStats)



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
    , querySoln : Maybe (Result T.SolveError T.USet)
    , sampleSyllables : List Syllable.Syll
    , sampleWords : List (List Syllable.Syll)
    , wordStats : WordStats
    }


init : a -> ( Model, Cmd Msg )
init _ =
    ( { examples = []
      , scramblishGrammar =
            { oldGrammar = en
            , newTitle = "Scramblish"
            , ruleMuts = []
            , wordMapping = Dict.empty
            , wordGenerator = WordGen.defaultWordGen
            }
      , querySoln = Nothing
      , sampleSyllables = []
      , sampleWords = []
      , wordStats = WordStats.default
      }
    , Cmd.batch
        [ generateScramblishGrammar
        , Utils.doCmd AddExample
        , Utils.doCmd AddExample
        , Utils.doCmd AddExample
        , Utils.doCmd RandomSyllables
        , Utils.doCmd RandomWords
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
    | RandomSolution T.SolnStream
    | RandomSyllables
    | RandomSyllablesGenerated (List Syllable.Syll)
    | RandomWords
    | RandomWordsGenerated (List (List Syllable.Syll))
    | HoverWord (Maybe String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ wordStats } as model) =
    case msg of
        AddExample ->
            ( model, randomSentences en model.scramblishGrammar "Sentence" )

        GeneratedExample syntaxTree ->
            let
                newExamples =
                    model.examples ++ [ syntaxTree ]

                newWordCounts =
                    WordStats.countWords newExamples
                        |> WordStats.mergeCounts
                            (newExamples
                                |> List.map (Mutation.mutateSyntaxTree model.scramblishGrammar)
                                |> WordStats.countWords
                            )
            in
            ( { model
                | examples = newExamples
                , wordStats = { wordStats | counts = newWordCounts }
              }
            , Cmd.none
            )

        MutateEnGrammar ->
            ( model, generateScramblishGrammar )

        MutationCreated grammarMut ->
            ( { model | scramblishGrammar = grammarMut }, Cmd.none )

        RandomSolve ->
            ( model
            , Random.generate
                RandomSolution
                (Logic.Solve.Randomized.solveQuery
                    (T.dbMerge Logic.Builtins.stdDb Logic.Builtins.stdDb)
                    T.emptyDupSubst
                    T.usetEmpty
                    ------
                    -- [ T.Comp "=" [ T.Var "X", T.Atom "asdf" ]
                    -- , T.Comp "=" [ T.Var "Y", T.Var "X" ]
                    -- ]
                    ------
                    -- [ T.Comp "append"
                    --     [ T.toValList [ T.Atom "a" ]
                    --     , T.toValList [ T.Atom "b", T.Atom "c" ]
                    --     , T.Var "What"
                    --     ]
                    -- ]
                    ------
                    [ T.Comp "phrase"
                        -- [ T.toValList [ T.Atom "x", T.Atom "y" ] ]
                        [ T.Comp
                            "they"
                            [ T.Atom "femm"
                            , T.Atom "sing"
                            , T.Atom "third"
                            ]

                        -- [ T.Var "G"
                        -- , T.Atom "N"
                        -- , T.Atom "P"
                        -- ]
                        , T.Var "Production"
                        ]
                    ]
                )
            )

        RandomSolution solns ->
            case Seq.next solns of
                Seq.Nil ->
                    ( { model | querySoln = Nothing }, Cmd.none )

                Seq.Cons (Ok ( _, u )) _ ->
                    ( { model | querySoln = Just (Ok u) }, Cmd.none )

                Seq.Cons (Err e) _ ->
                    ( { model | querySoln = Just (Err e) }, Cmd.none )

        RandomSyllables ->
            ( model
            , Random.generate
                RandomSyllablesGenerated
                (Random.list 50 (Syllable.randomSyllable model.scramblishGrammar.wordGenerator.phonology))
            )

        RandomSyllablesGenerated syllables ->
            ( { model | sampleSyllables = syllables }
            , Cmd.none
            )

        RandomWords ->
            ( model
            , Random.generate
                RandomWordsGenerated
                (Random.list 25 (WordGen.randomWordIpa model.scramblishGrammar.wordGenerator.phonology))
            )

        RandomWordsGenerated words ->
            ( { model | sampleWords = words }, Cmd.none )

        HoverWord mWord ->
            let
                ws =
                    model.wordStats
            in
            ( { model | wordStats = { ws | hoveredWord = mWord } }
            , Cmd.none
            )


randomSentences : Grammar -> GrammarMut -> String -> Cmd Msg
randomSentences eng _ start =
    Random.generate
        GeneratedExample
        (generateSyntaxTree eng (Nt start))


generateScramblishGrammar : Cmd Msg
generateScramblishGrammar =
    Random.generate MutationCreated
        (WordGen.randomWordGen
            |> Random.andThen
                (\wordGen ->
                    Mutation.grammarMutGenerator "Scramblish" wordGen en
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
            [ details [ attribute "open" "false" ]
                (summary [] [ text "Sentence Examples" ]
                    :: (model.examples
                            |> List.indexedMap
                                (sentenceExampleView
                                    model.wordStats
                                    model.scramblishGrammar
                                )
                       )
                    ++ [ button [ onClick AddExample ] [ text "+ Additional Example" ] ]
                )
            , details [ attribute "open" "true" ]
                [ summary [] [ text "Word Generation" ]
                , button [ onClick MutateEnGrammar ] [ text "⟳ Regenerate Scramblish" ]
                , WordGen.viewPhonology model.scramblishGrammar.wordGenerator.phonology
                , button [ onClick RandomSyllables ] [ text "Random Syllables" ]
                , p []
                    (model.sampleSyllables
                        |> List.map
                            (\s ->
                                span [ class "sample-syllable" ]
                                    [ text "/\u{2060}", Syllable.viewSyllable s, text "\u{2060}/ " ]
                            )
                    )
                , button [ onClick RandomWords ] [ text "Random Words" ]
                , p []
                    (model.sampleWords
                        |> List.map
                            (\w ->
                                span [ class "sample-word", class "ipa" ]
                                    [ text "/\u{2060}"
                                    , WordGen.viewWord w
                                    , text "\u{2060}/ "
                                    , span [ class "sample-word", class "orthography" ]
                                        [ text (WordGen.Ortho.applyOrthoMappingToWordWithMarkers model.scramblishGrammar.wordGenerator.orthography w)
                                        , text " "
                                        ]
                                    ]
                            )
                    )
                ]
            , details []
                (summary [] [ text "Query Tests" ]
                    :: button [ onClick RandomSolve ] [ text "Random Solve Query" ]
                    :: (case model.querySoln of
                            Nothing ->
                                [ text "No query results yet." ]

                            Just (Ok u) ->
                                [ text "Query succeeded: ", viewUSet u ]

                            Just (Err e) ->
                                [ text ("Query failed: " ++ Debug.toString e) ]
                       )
                )

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


viewUSet : T.USet -> Html msg
viewUSet u =
    u
        |> T.simplifyUSet
        -- Skip over variables that contain '#' (internal variables).
        -- |> Dict.filter (\k _ -> String.contains "#" k |> not)
        |> Dict.foldl
            (\varName value acc ->
                acc
                    ++ [ div []
                            [ text varName
                            , text " = "
                            , text (T.stringOfVal value)
                            ]
                       ]
            )
            []
        |> div []


sentenceExampleView : WordStats -> GrammarMut -> Int -> SyntaxTree -> Html Msg
sentenceExampleView wordStats grammarMut index engTree =
    let
        scrTree =
            mutateSyntaxTree grammarMut engTree
    in
    div [ style "padding-block" "1rem", class "example" ]
        [ h3 [] [ text ("Example " ++ String.fromInt (index + 1)) ]
        , dl [ class "translation" ]
            [ div []
                [ dt [] [ text "Scramblish:" ]
                , dd [] [ syntaxTreeView wordStats grammarMut.wordGenerator.orthography.title scrTree ]
                ]
            , div []
                [ dt [] [ text "English:" ]
                , dd [] [ syntaxTreeView wordStats "english" engTree ]
                ]
            ]
        ]


syntaxTreeView : WordStats -> String -> SyntaxTree -> Html Msg
syntaxTreeView wordStats scriptName tree =
    syntaxTreeToWordList tree
        |> List.map (viewWord wordStats)
        |> List.intersperse (span [ class "whitespace" ] [ text " " ])
        |> span [ class "sentence", class ("script-name--" ++ String.join "-" (String.split " " scriptName)) ]


viewWord : WordStats -> String -> Html Msg
viewWord wordStats word =
    let
        subscript =
            WordStats.getCountForWord wordStats word
                |> String.fromInt
                |> text
                |> List.singleton
                |> span
                    [ class "word-count"

                    -- Prevent selection of the count. Helpful to exclude
                    -- numbers while copy-pasting.
                    , attribute "inert" ""
                    ]

        classes =
            if wordStats.hoveredWord == Just word then
                [ class "word hovered"
                , onMouseOut (HoverWord Nothing)
                ]

            else
                [ class "word"
                , onMouseOver (HoverWord (Just word))
                ]
    in
    span
        [ class "word-and-subscript" ]
        [ span classes [ text word ]
        , subscript
        ]
