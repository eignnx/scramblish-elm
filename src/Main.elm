module Main exposing (..)

import Browser
import Dict
import EnGrammar exposing (..)
import Grammar exposing (..)
import Html exposing (Html, aside, button, dd, details, div, dl, dt, footer, h1, h3, header, main_, p, section, span, summary, table, td, text, th, tr)
import Html.Attributes exposing (alt, attribute, class, id, src, style, title)
import Html.Events exposing (onClick, onMouseOut, onMouseOver, stopPropagationOn)
import Json.Decode as De
import List.Extra
import Logic.Builtins
import Logic.Solve.Randomized
import Logic.Types as T
import Mutation exposing (GrammarMut, mutateSyntaxTree)
import Platform.Cmd as Cmd exposing (Cmd)
import Random
import Req exposing (..)
import Seq
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
    , answerCheckingMode : Bool
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
      , answerCheckingMode = False
      }
    , Cmd.batch
        [ generateScramblishGrammar
        , Utils.doCmd (RandomExample (Ask ()))
        , Utils.doCmd (RandomExample (Ask ()))
        , Utils.doCmd (RandomExample (Ask ()))
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = RandomExample (Req () SyntaxTree)
    | MutateEnGrammar (Req () Mutation.GrammarMut)
    | RandomSolve (Req () T.SolnStream)
    | RandomSyllables (Req () (List Syllable.Syll))
    | RandomWords (Req () (List (List Syllable.Syll)))
    | HoverWord (Maybe ( Lang, String ))
    | SelectWord (Maybe ( Lang, String ))
    | DeleteTranslationPair { eng : String, scr : String }
    | ToggleCheckAnswers


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ wordStats } as model) =
    case msg of
        RandomExample (Ask ()) ->
            ( model, randomSentences en model.scramblishGrammar "Sentence" )

        RandomExample (Answer syntaxTree) ->
            let
                newExamples =
                    model.examples ++ [ syntaxTree ]

                wordCounts =
                    computeWordStats model.scramblishGrammar newExamples
            in
            ( { model
                | examples = newExamples
                , wordStats =
                    { wordStats
                        | engCounts = wordCounts.eng
                        , scrCounts = wordCounts.scr
                    }
              }
            , Cmd.none
            )

        MutateEnGrammar (Ask ()) ->
            ( model, generateScramblishGrammar )

        MutateEnGrammar (Answer grammarMut) ->
            let
                wordCounts =
                    computeWordStats grammarMut model.examples
            in
            ( { model
                | scramblishGrammar = grammarMut
                , wordStats =
                    { wordStats
                        | engCounts = wordCounts.eng
                        , scrCounts = wordCounts.scr
                    }
              }
            , Cmd.batch
                [ Utils.doCmd (RandomSyllables (Ask ()))
                , Utils.doCmd (RandomWords (Ask ()))
                ]
            )

        RandomSolve (Ask ()) ->
            ( model
            , Random.generate
                (Answer >> RandomSolve)
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

        RandomSolve (Answer solns) ->
            case Seq.next solns of
                Seq.Nil ->
                    ( { model | querySoln = Nothing }, Cmd.none )

                Seq.Cons (Ok ( _, u )) _ ->
                    ( { model | querySoln = Just (Ok u) }, Cmd.none )

                Seq.Cons (Err e) _ ->
                    ( { model | querySoln = Just (Err e) }, Cmd.none )

        RandomSyllables (Ask ()) ->
            ( model
            , Random.generate
                (Answer >> RandomSyllables)
                (Random.list 50 (Syllable.randomSyllable model.scramblishGrammar.wordGenerator.phonology))
            )

        RandomSyllables (Answer syllables) ->
            ( { model | sampleSyllables = syllables }
            , Cmd.none
            )

        RandomWords (Ask ()) ->
            ( model
            , Random.generate
                (Answer >> RandomWords)
                (Random.list 25 (WordGen.randomWordIpa model.scramblishGrammar.wordGenerator.phonology))
            )

        RandomWords (Answer words) ->
            ( { model | sampleWords = words }, Cmd.none )

        HoverWord mWord ->
            let
                ws =
                    model.wordStats
            in
            ( { model | wordStats = { ws | hoveredWord = mWord } }
            , Cmd.none
            )

        SelectWord mWord ->
            case ( model.wordStats.selectedWord, mWord ) of
                ( Nothing, Nothing ) ->
                    ( model, Cmd.none )

                ( Nothing, Just _ ) ->
                    ( { model | wordStats = { wordStats | selectedWord = mWord } }
                    , Cmd.none
                    )

                ( Just _, Nothing ) ->
                    ( { model | wordStats = { wordStats | selectedWord = Nothing } }
                    , Cmd.none
                    )

                ( Just ( lang1, word1 ), Just ( lang2, word2 ) ) ->
                    if lang1 /= lang2 then
                        let
                            ( engWord, scrWord ) =
                                if lang1 == English then
                                    ( word1, word2 )

                                else
                                    ( word2, word1 )
                        in
                        ( { model
                            | wordStats =
                                { wordStats
                                    | selectedWord = Nothing
                                    , userTranslations =
                                        { eng = engWord, scr = scrWord }
                                            :: wordStats.userTranslations
                                }
                          }
                        , Cmd.none
                        )

                    else if word1 == word2 then
                        ( { model
                            | wordStats =
                                { wordStats
                                    | selectedWord = Nothing
                                    , hoveredWord = Just ( lang1, word1 )
                                }
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | wordStats = { wordStats | selectedWord = mWord } }
                        , Cmd.none
                        )

        DeleteTranslationPair { eng, scr } ->
            ( { model
                | wordStats =
                    { wordStats
                        | userTranslations =
                            List.filter
                                (\other -> not (other.eng == eng && other.scr == scr))
                                wordStats.userTranslations
                    }
              }
            , Cmd.none
            )

        ToggleCheckAnswers ->
            ( { model | answerCheckingMode = not model.answerCheckingMode }
            , Cmd.none
            )


randomSentences : Grammar -> GrammarMut -> String -> Cmd Msg
randomSentences eng _ start =
    Random.generate
        (Answer >> RandomExample)
        (generateSyntaxTree eng (Nt start))


computeWordStats :
    GrammarMut
    -> List SyntaxTree
    -> { eng : Dict.Dict String Int, scr : Dict.Dict String Int }
computeWordStats scramblishGrammar sentences =
    let
        engCounts =
            WordStats.countWords sentences

        scrCounts =
            sentences
                |> List.map (Mutation.mutateSyntaxTree scramblishGrammar)
                |> WordStats.countWords
    in
    { eng = engCounts, scr = scrCounts }


generateScramblishGrammar : Cmd Msg
generateScramblishGrammar =
    Random.generate (Answer >> MutateEnGrammar)
        (WordGen.randomWordGen
            |> Random.andThen
                (\wordGen ->
                    Mutation.grammarMutGenerator "Scramblish" wordGen en
                )
        )



-- VIEW


view : Model -> Html Msg
view model =
    let
        incorrectTranslations =
            if model.answerCheckingMode then
                model.wordStats.userTranslations
                    |> List.filter
                        (\{ eng, scr } ->
                            Dict.get eng model.scramblishGrammar.wordMapping /= Just scr
                        )

            else
                []
    in
    div [ id "app-outer-wrapper" ]
        [ viewSidePanel model incorrectTranslations
        , div
            [ id "app-content"
            , onClick (SelectWord Nothing)
            ]
            [ header []
                [ h1 []
                    [ text "Scramblish" ]
                ]
            , main_ []
                [ details [ attribute "open" "true" ]
                    (summary [] [ text "Sentence Examples" ]
                        :: (model.examples
                                |> List.indexedMap
                                    (sentenceExampleView
                                        model
                                        incorrectTranslations
                                        model.scramblishGrammar
                                    )
                           )
                        ++ [ button [ onClick (RandomExample (Ask ())) ] [ text "+ Additional Example" ] ]
                    )
                , details []
                    [ summary [] [ text "Word Generation" ]
                    , button [ onClick (MutateEnGrammar (Ask ())) ] [ text "⟳ Regenerate Scramblish" ]
                    , WordGen.viewPhonology model.scramblishGrammar.wordGenerator.phonology
                    , button [ onClick (RandomSyllables (Ask ())) ] [ text "Random Syllables" ]
                    , p []
                        (model.sampleSyllables
                            |> List.map
                                (\s ->
                                    span [ class "sample-syllable" ]
                                        [ text "/\u{2060}", Syllable.viewSyllable s, text "\u{2060}/ " ]
                                )
                        )
                    , button [ onClick (RandomWords (Ask ())) ] [ text "Random Words" ]
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
                        :: button [ onClick (RandomSolve (Ask ())) ] [ text "Random Solve Query" ]
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
                    [ button [ onClick (MutateEnGrammar (Ask ())) ] [ text "⟳ Regenerate Grammar Mutation" ]
                    , renderGrammar <|
                        Mutation.applyGrammarMut model.scramblishGrammar
                    ]
                ]
            , footer []
                [ text "© 2024" ]
            ]
        ]


viewSidePanel model incorrectTranslations =
    viewUserTranslations
        { wordStats = model.wordStats
        , incorrectTranslations = incorrectTranslations
        }
        model.wordStats.userTranslations
        { buttons =
            [ \_ attrs ->
                button
                    (attrs
                        ++ [ id "answer-checking-toggle"
                           , title
                                (if model.answerCheckingMode then
                                    "Answers Revealed"

                                 else
                                    "Check Answers"
                                )
                           , class
                                (if model.answerCheckingMode then
                                    "answer-checking-on"

                                 else
                                    "answer-checking-off"
                                )
                           , onClick ToggleCheckAnswers
                           ]
                    )
                    [ text
                        (if model.answerCheckingMode then
                            "👀"

                         else
                            "🕶"
                        )
                    ]
            ]
        }


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


sentenceExampleView : Model -> List { eng : String, scr : String } -> GrammarMut -> Int -> SyntaxTree -> Html Msg
sentenceExampleView model incorrectTranslations grammarMut index engTree =
    let
        scrTree =
            mutateSyntaxTree grammarMut engTree
    in
    div [ style "padding-block" "1rem", class "example" ]
        [ h3 [] [ text ("Example " ++ String.fromInt (index + 1)) ]
        , dl [ class "translation" ]
            [ div []
                [ dt [] [ text "Scramblish:" ]
                , dd []
                    [ syntaxTreeView
                        model
                        incorrectTranslations
                        grammarMut.wordGenerator.orthography.title
                        Scramblish
                        scrTree
                    ]
                ]
            , div []
                [ dt [] [ text "English:" ]
                , dd []
                    [ syntaxTreeView
                        model
                        incorrectTranslations
                        "english"
                        English
                        engTree
                    ]
                ]
            ]
        ]


syntaxTreeView : Model -> List { eng : String, scr : String } -> String -> Lang -> SyntaxTree -> Html Msg
syntaxTreeView { answerCheckingMode, wordStats } incorrectTranslations scriptName lang tree =
    syntaxTreeToWordList tree
        |> List.map
            (\word ->
                viewWord
                    { displayRuby = True
                    , wordIsIncorrectTranslation =
                        answerCheckingMode
                            && List.member word
                                (case lang of
                                    English ->
                                        List.map .eng incorrectTranslations

                                    Scramblish ->
                                        List.map .scr incorrectTranslations
                                )
                    , wordStats = wordStats
                    }
                    lang
                    word
            )
        |> List.intersperse (span [ class "whitespace" ] [ text " " ])
        |> span [ class "sentence", class ("script-name--" ++ String.join "-" (String.split " " scriptName)) ]


type alias ViewWordProps =
    { displayRuby : Bool
    , wordIsIncorrectTranslation : Bool
    , wordStats : WordStats
    }


viewWord : ViewWordProps -> Lang -> String -> Html Msg
viewWord { displayRuby, wordStats, wordIsIncorrectTranslation } lang word =
    let
        count =
            WordStats.getCountForWord wordStats lang word

        subscript =
            if count > 0 then
                count
                    |> String.fromInt
                    |> text
                    |> List.singleton
                    |> span
                        [ class "word-count"

                        -- Prevent selection of the count. Helpful to exclude
                        -- numbers while copy-pasting.
                        , attribute "inert" ""
                        ]

            else
                span [] []

        userTranslationPartner : List String
        userTranslationPartner =
            case lang of
                English ->
                    List.filter (\{ eng } -> eng == word) wordStats.userTranslations
                        |> List.map .scr

                Scramblish ->
                    List.filter (\{ scr } -> scr == word) wordStats.userTranslations
                        |> List.map .eng

        attrs =
            class "word"
                :: (if wordStats.selectedWord == Just ( lang, word ) then
                        [ class "selected" ]

                    else if wordStats.hoveredWord == Just ( lang, word ) then
                        [ class "hovered"
                        , onMouseOut (HoverWord Nothing)
                        , onClickNoPropogate (SelectWord (Just ( lang, word )))
                        ]

                    else
                        [ onMouseOver (HoverWord (Just ( lang, word )))
                        , onClickNoPropogate (SelectWord (Just ( lang, word )))
                        ]
                   )
                ++ (if
                        List.Extra.find
                            (\{ eng, scr } -> eng == word || scr == word)
                            wordStats.userTranslations
                            /= Nothing
                    then
                        [ class "translated" ]

                    else
                        []
                   )
                ++ (if wordIsIncorrectTranslation then
                        [ class "incorrect-translation" ]

                    else
                        []
                   )
    in
    case userTranslationPartner of
        [] ->
            span [ class "word-and-subscript" ]
                [ span attrs [ text word ]
                , subscript
                ]

        [ partner ] ->
            if displayRuby then
                Html.ruby
                    [ class "word-and-subscript" ]
                    [ span attrs [ text word ]
                    , subscript
                    , Html.rt
                        [ class "translation" ]
                        [ text partner ]
                    ]

            else
                span [ class "word-and-subscript" ]
                    [ span attrs [ text word ]
                    , subscript
                    ]

        partners ->
            if displayRuby then
                let
                    options =
                        partners
                            |> List.sort
                            |> List.Extra.subsequences
                            |> List.filter (List.isEmpty >> not)
                            |> List.reverse
                            |> List.map List.sort
                            |> List.map
                                (\group ->
                                    Html.option
                                        (Html.Attributes.value (String.join ", " group)
                                            :: (if List.length group == 1 then
                                                    [ class "unique-homonym-option" ]

                                                else
                                                    [ class "multi-homonym-option" ]
                                               )
                                        )
                                        [ text (String.join ", " group) ]
                                )
                in
                Html.ruby
                    [ class "word-and-subscript" ]
                    [ span (class "homonym" :: attrs) [ text word ]
                    , subscript
                    , Html.rt
                        [ class "translation" ]
                        [ Html.select [] options ]
                    ]

            else
                span [ class "word-and-subscript" ]
                    [ span attrs [ text word ]
                    , subscript
                    ]


type alias ViewUserTranslationsProps =
    { wordStats : WordStats
    , incorrectTranslations : List { eng : String, scr : String }
    }


viewUserTranslations :
    ViewUserTranslationsProps
    -> List { eng : String, scr : String }
    -> { buttons : List (Int -> List (Html.Attribute Msg) -> Html Msg) }
    -> Html Msg
viewUserTranslations { wordStats, incorrectTranslations } userTranslations { buttons } =
    let
        viewRow { eng, scr } =
            let
                viewWordProps : ViewWordProps
                viewWordProps =
                    { displayRuby = False
                    , wordStats = wordStats
                    , wordIsIncorrectTranslation =
                        List.member { eng = eng, scr = scr } incorrectTranslations
                    }
            in
            tr []
                [ td [] [ viewWord viewWordProps English eng ]
                , td [] [ viewWord viewWordProps Scramblish scr ]
                , td []
                    [ button
                        [ title "Delete the translation pair"
                        , onClick
                            (DeleteTranslationPair { eng = eng, scr = scr })
                        ]
                        [ Html.img
                            [ src "assets/delete_24dp_GOLDENROD_FILL0_wght400_GRAD0_opsz24.svg"
                            , alt "Delete translation pair"
                            ]
                            []
                        ]
                    ]
                ]
    in
    aside
        [ id "side-panel" ]
        [ div [ id "wrap-side-btns" ]
            (Html.label [ id "side-panel-toggle-label", class "side-panel-btn" ]
                [ Html.input
                    [ Html.Attributes.type_ "checkbox"
                    , id "side-panel-toggle"
                    ]
                    []
                ]
                :: List.indexedMap (\idx btn -> btn idx [ class "side-panel-btn" ]) buttons
            )
        , div [ id "user-translations-table-wrap" ]
            [ table []
                (tr []
                    [ th [] [ text "English" ]
                    , th [] [ text "Scramblish" ]
                    , th [] []
                    ]
                    :: (case wordStats.selectedWord of
                            Just ( English, eng ) ->
                                [ viewRow { eng = eng, scr = "—" } ]

                            Just ( Scramblish, scr ) ->
                                [ viewRow { eng = "—", scr = scr } ]

                            Nothing ->
                                []
                       )
                    ++ (if List.isEmpty userTranslations && wordStats.selectedWord == Nothing then
                            [ tr []
                                [ td [] [ text "—" ]
                                , td [] [ text "—" ]
                                , td [] [ text "—" ]
                                ]
                            ]

                        else
                            userTranslations |> List.map viewRow
                       )
                )
            ]
        ]


onClickNoPropogate : msg -> Html.Attribute msg
onClickNoPropogate msg =
    stopPropagationOn "click" (De.succeed ( msg, True ))
