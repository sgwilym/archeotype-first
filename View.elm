module View exposing (..)

import Html exposing (Html)
import Html.Attributes
import Attempt exposing (Attempt)
import Problem exposing (Problem)
import Board exposing (Board, Cell(..))
import String
import Letter exposing (Letter)
import Cons exposing (Cons)


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
                    String.fromChar (Letter.toChar letter) ++ "\n"
                 else
                    String.fromChar (Letter.toChar letter)
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
        , problemAttempt problem
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
            (toString (List.length (Cons.filter Problem.isIncomplete problems)) ++ " problem(s) remaining")
        ]
