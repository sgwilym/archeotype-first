module View exposing (..)

import Html exposing (Html)
import Html.Attributes
import Attempt exposing (Attempt)
import Problem exposing (Problem)
import Board exposing (Board, Cell(..))
import String
import Letter exposing (Letter)
import Cons


type Colour
    = Black
    | Red
    | Green


board : Board -> Html msg
board board =
    Html.pre []
        (Cons.toList
            (Cons.indexedMap
                (\index cell' ->
                    cell
                        (rem (index + 1) board.width == 0)
                        cell'
                )
                board.cells
            )
        )


cell : Bool -> Cell -> Html msg
cell lastInRow cell =
    case cell of
        Cell letter ->
            Html.text
                (if lastInRow then
                    toString letter ++ "\n"
                 else
                    toString letter
                )

        EmptyCell ->
            Html.text
                ((if lastInRow then
                    " \n"
                  else
                    " "
                 )
                )


problem : Problem -> Html msg
problem problem =
    Html.div []
        [ hint problem.hint
        , maybeAttempt problem.attempt
        ]


hint : String -> Html msg
hint hint =
    Html.div []
        [ Html.text hint
        ]



-- NEXT: Make a nice recursive function out of this


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
            [ attemptLetter Black letter ]


maybeAttempt : Maybe Attempt -> Html msg
maybeAttempt attempt' =
    case attempt' of
        Just justAttempt ->
            Html.div []
                (attempt justAttempt)

        Nothing ->
            Html.div []
                [ Html.text "Start typing!" ]
