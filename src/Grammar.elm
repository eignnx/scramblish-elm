module Grammar exposing (..)

import Html exposing (Html, a, dd, div, dl, dt, h2, span, text)
import Html.Attributes exposing (class, href, id)
import Random
import Set
import Utils


type alias Grammar =
    { title : String
    , rules : List ( Nt, SententialForm )
    }


type Nt
    = Nt String


type alias SententialForm =
    List Form


type Form
    = NtForm Nt
    | TmForm Tm


type Tm
    = Tm String



-- GENERATE SENTENCES


type SyntaxTree
    = Leaf Tm
    | Node Nt (List SyntaxTree)


lookupNt : List ( Nt, SententialForm ) -> Nt -> List SententialForm
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
        rules : List SententialForm
        rules =
            lookupNt grammar.rules start

        chooseSententialform : Random.Generator SententialForm
        chooseSententialform =
            case rules of
                [] ->
                    Random.constant [ TmForm (Tm ("<" ++ Debug.toString start ++ ">")) ]

                x :: xs ->
                    Random.uniform x xs

        mapForm : Form -> Random.Generator SyntaxTree
        mapForm form =
            case form of
                NtForm nt ->
                    generateSyntaxTree grammar nt

                TmForm tm ->
                    Random.map Leaf (Random.constant tm)

        makeSyntaxTree : SententialForm -> Random.Generator SyntaxTree
        makeSyntaxTree sententialForm =
            Random.map (Node start) (Utils.randomFlattenList mapForm sententialForm)
    in
    chooseSententialform
        |> Random.andThen makeSyntaxTree



-- RENDERING


renderGrammar : Grammar -> Html msg
renderGrammar { title, rules } =
    div []
        [ h2 [ class "grammar-title" ] [ text title ]
        , dl [ class "grammar-rules" ] (groupByNt rules |> List.map renderRule)
        ]


renderRule : ( Nt, List SententialForm ) -> Html msg
renderRule ( Nt name, sententialForms ) =
    div [ class "grammar-rule" ]
        (dt [ id ("h-" ++ name) ] [ text name ]
            :: List.map renderSententialForm sententialForms
        )


renderSententialForm : SententialForm -> Html msg
renderSententialForm sententialForm =
    dd [] (List.map renderForm sententialForm)


renderForm : Form -> Html msg
renderForm form =
    case form of
        NtForm nt ->
            renderNt nt

        TmForm tm ->
            renderTm tm


renderNt : Nt -> Html msg
renderNt (Nt name) =
    a [ class "nonterminal", href ("#h-" ++ name) ] [ text name ]


renderTm : Tm -> Html msg
renderTm (Tm tm) =
    span [ class "terminal" ] [ text tm ]


syntaxTreeView : SyntaxTree -> Html msg
syntaxTreeView tree =
    syntaxTreeToWordList tree
        |> List.map (\word -> span [ class "word" ] [ text word ])
        |> List.intersperse (span [ class "whitespace" ] [ text " " ])
        |> span [ class "sentence" ]


syntaxTreeToWordList : SyntaxTree -> List String
syntaxTreeToWordList tree =
    case tree of
        Leaf (Tm word) ->
            [ word ]

        Node _ subtrees ->
            subtrees
                |> List.map syntaxTreeToWordList
                |> List.concat
