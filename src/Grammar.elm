module Grammar exposing (..)

import Html exposing (Html, a, dd, details, div, dl, dt, span, summary, text)
import Html.Attributes exposing (attribute, class, href, id)
import Random
import Set
import Utils


type alias Grammar =
    { title : String
    , rules : List ( Nt, SententialForm )
    }


{-| A non-terminal symbol.
-}
type Nt
    = Nt String


type alias SententialForm =
    List Form


type Form
    = NtForm Nt
    | TmForm Tm


{-| A terminal symbol.
-}
type Tm
    = Tm String


type alias Translation a =
    { eng : a, scr : a }



-- GENERATE SENTENCES


type SyntaxTree
    = Leaf Tm
    | Node
        { nt : Nt
        , branchIndex : Int
        , children : List SyntaxTree
        }


lookupNt : List ( Nt, a ) -> Nt -> List a
lookupNt rules tgtNt =
    let
        findRule ( nt, rhss ) =
            if tgtNt == nt then
                Just rhss

            else
                Nothing
    in
    List.filterMap findRule rules


groupByNt : List ( Nt, SententialForm ) -> List ( Nt, List SententialForm )
groupByNt rules =
    let
        uniqueNtNames =
            rules
                |> List.map (\( Nt nt, _ ) -> nt)
                |> Set.fromList
                |> Set.toList
                |> List.map Nt
    in
    List.map (\nt -> ( nt, lookupNt rules nt )) uniqueNtNames


generateSyntaxTree : Grammar -> Nt -> Random.Generator SyntaxTree
generateSyntaxTree grammar start =
    let
        clauses : List SententialForm
        clauses =
            lookupNt grammar.rules start

        chooseSententialform : Random.Generator ( Int, SententialForm )
        chooseSententialform =
            case Utils.indexed clauses of
                [] ->
                    Random.constant ( 0, [ TmForm (Tm ("<" ++ Debug.toString start ++ ">")) ] )

                x :: xs ->
                    Random.uniform x xs

        mapForm : Form -> Random.Generator SyntaxTree
        mapForm form =
            case form of
                NtForm nt ->
                    generateSyntaxTree grammar nt

                TmForm tm ->
                    Random.map Leaf (Random.constant tm)

        makeSyntaxTree : ( Int, SententialForm ) -> Random.Generator SyntaxTree
        makeSyntaxTree ( branchIndex, sententialForm ) =
            Utils.randomFlattenList mapForm sententialForm
                |> Random.map
                    (\children ->
                        Node
                            { nt = start
                            , branchIndex = branchIndex
                            , children = children
                            }
                    )
    in
    chooseSententialform
        |> Random.andThen makeSyntaxTree



-- RENDERING


renderGrammar : Grammar -> Html msg
renderGrammar { title, rules } =
    details [ attribute "open" "true" ]
        [ summary [ class "grammar-title" ] [ text title ]
        , dl [ class "grammar-rules" ] (groupByNt rules |> List.map (renderRule title))
        ]


renderRule : String -> ( Nt, List SententialForm ) -> Html msg
renderRule title ( Nt name, sententialForms ) =
    div [ class "grammar-rule" ]
        (dt [ id ("h-" ++ title ++ "-" ++ name) ] [ text name ]
            :: List.map (renderSententialForm title) sententialForms
        )


renderSententialForm : String -> SententialForm -> Html msg
renderSententialForm title sententialForm =
    dd [] (List.map (renderForm title) sententialForm)


renderForm : String -> Form -> Html msg
renderForm title form =
    case form of
        NtForm nt ->
            renderNt title nt

        TmForm tm ->
            renderTm tm


renderNt : String -> Nt -> Html msg
renderNt title (Nt name) =
    a [ class "nonterminal", href ("#h-" ++ title ++ "-" ++ name) ] [ text name ]


renderTm : Tm -> Html msg
renderTm (Tm tm) =
    span [ class "terminal" ] [ text tm ]


syntaxTreeView : String -> SyntaxTree -> Html msg
syntaxTreeView scriptName tree =
    syntaxTreeToWordList tree
        |> List.map (\word -> span [ class "word" ] [ text word ])
        |> List.intersperse (span [ class "whitespace" ] [ text " " ])
        |> span [ class "sentence", class ("script-name--" ++ scriptName) ]


syntaxTreeToWordList : SyntaxTree -> List String
syntaxTreeToWordList tree =
    case tree of
        Leaf (Tm word) ->
            [ word ]

        Node { children } ->
            children
                |> List.map syntaxTreeToWordList
                |> List.concat
