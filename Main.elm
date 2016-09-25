module Main exposing (..)

import Html exposing (Html)
import Html.App
import Cons exposing (Cons, cons)
import Key exposing (Key)
import Letter exposing (Letter(..))
import Puzzle exposing (Puzzle)
import Keyboard exposing (KeyCode)
import View
import Board


type Msg
    = KeyMsg KeyCode


myPuzzle : Cons ( String, Cons Letter )
myPuzzle =
    cons ( "A stripey horse", cons Z [ E, B, R, A ] )
        [ ( "A nationalistic root vegetable", cons C [ A, R, R, O, T ] )
        , ( "A naughty kot!", cons R [ O, K, K, A, K, U ] )
        , ( "A two-wheeled horse", cons B [ I, C, Y, C, L, E ] )
        , ( "A Dutch illustrator", cons W [ E, S, T, E, N, D, O, R, P ] )
        ]


model : Puzzle
model =
    Puzzle.create myPuzzle


update : Msg -> Puzzle -> ( Puzzle, Cmd a )
update msg model =
    case msg of
        KeyMsg keycode ->
            ( Puzzle.update (Key.fromKeyCode keycode) model, Cmd.none )


subscriptions : Puzzle -> Sub Msg
subscriptions model =
    Keyboard.presses KeyMsg


view : Puzzle -> Html a
view model =
    case model of
        Puzzle.InProgress problems ->
            Html.div []
                [ View.board (Board.fromPuzzle model 20 69)
                , View.problem (Cons.head problems)
                ]

        Puzzle.Finished ->
            Html.text "You did it!"


main : Program Never
main =
    Html.App.program
        { init = ( model, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
