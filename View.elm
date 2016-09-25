module View exposing (..)

import Html exposing (Html)
import Attempt exposing (Attempt)
import Problem exposing (Problem)
import Board exposing (Board, Cell(..))
import Cons


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
        , attempt problem.attempt
        ]


hint : String -> Html msg
hint hint =
    Html.div []
        [ Html.text hint
        ]


attempt : Maybe Attempt -> Html msg
attempt attempt =
    case attempt of
        Just attempt' ->
            Html.div []
                [ Html.text (toString (Attempt.toLetters attempt'))
                ]

        Nothing ->
            Html.div [] [ Html.text "Start typing!" ]
