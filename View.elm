module View exposing (..)

import Html exposing (Html)
import Html.Attributes
import Attempt exposing (Attempt)
import Problem exposing (Problem)
import Board exposing (Board, Cell(..))
import String
import Letter exposing (Letter)
import Cons exposing (Cons)
import List.Split exposing (..)
import Html.Attributes exposing (style)


type Colour
    = Black
    | Red
    | Green


rowStyle : Html.Attribute a
rowStyle =
    style
        []


cellStyle : Html.Attribute a
cellStyle =
    style [ ( "width", "50px" ), ( "height", "50px" ), ( "text-align", "center" ) ]


coveredCellStyle : Html.Attribute a
coveredCellStyle =
    style [ ( "color", "white" ), ( "background-color", "black" ) ]


board : Board -> Html msg
board board =
    let
        rows =
            chunksOfLeft board.width (Cons.toList board.cells)
    in
        Html.table []
            (List.map
                (\row ->
                    Html.tr [] (List.map cell row)
                )
                rows
            )


cell : Cell -> Html msg
cell cell =
    case cell of
        Cell letter ->
            Html.td
                [ cellStyle, coveredCellStyle ]
                [ Html.text (String.fromChar (Letter.toChar letter)) ]

        EmptyCell ->
            Html.td
                [ cellStyle ]
                [ Html.text "\t" ]


problem : Problem -> Html msg
problem problem =
    Html.div []
        [ hint problem.hint
        , problemAttempt problem
        ]


hint : String -> Html msg
hint hint =
    Html.div []
        [ Html.text hint
        ]


attemptLetter : Colour -> Letter -> Html msg
attemptLetter colour letter =
    let
        colorValue =
            case colour of
                Black ->
                    "black"

                Red ->
                    "red"

                Green ->
                    "green"
    in
        Html.span [ Html.Attributes.style [ ( "color", colorValue ) ] ]
            [ Html.text (String.fromChar (Letter.toChar letter)) ]


attempt : Attempt -> List (Html msg)
attempt attempt' =
    case attempt' of
        Attempt.Success letter nextAttempt ->
            case Attempt.last attempt' of
                Attempt.Complete _ ->
                    [ attemptLetter Green letter ] ++ attempt nextAttempt

                _ ->
                    [ attemptLetter Black letter ] ++ attempt nextAttempt

        Attempt.Failure letter ->
            [ attemptLetter Red letter ]

        Attempt.Complete letter ->
            [ attemptLetter Green letter ]

        Attempt.Incomplete letter ->
            [ attemptLetter Green letter ]


problemAttempt : Problem -> Html msg
problemAttempt problem =
    case problem.attempt of
        Just attempt' ->
            let
                remainingNumberOfLetters =
                    Cons.length (problem.answer)
                        - Attempt.length attempt'
            in
                Html.div []
                    ((attempt attempt')
                        ++ (List.repeat remainingNumberOfLetters (Html.span [] [ Html.text "?" ]))
                    )

        Nothing ->
            Html.div []
                (List.repeat (Cons.length problem.answer) (Html.span [] [ Html.text "?" ]))


problemsRemaining : Cons Problem -> Html msg
problemsRemaining problems =
    Html.div []
        [ Html.text
            ((toString (Cons.length problems)) ++ " problem(s) remaining")
        ]
