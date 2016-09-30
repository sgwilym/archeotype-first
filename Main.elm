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
import Http
import Task


type Msg
    = KeyMsg KeyCode
    | FetchSucceed (Cons ( String, Cons Letter ))
    | FetchFail Http.Error


myPuzzle : Cons ( String, Cons Letter )
myPuzzle =
    cons ( "A stripey horse", cons Z [ E, B, R, A ] )
        [ ( "A nationalistic root vegetable", cons C [ A, R, R, O, T ] )
        , ( "A naughty kot!", cons R [ O, K, K, A, K, U ] )
        , ( "A two-wheeled horse", cons B [ I, C, Y, C, L, E ] )
        , ( "A Dutch illustrator", cons W [ E, S, T, E, N, D, O, R, P ] )
        ]



-- TODO: Expand model with fetching, failed states…


model : Puzzle
model =
    Puzzle.create myPuzzle


update : Msg -> Puzzle -> ( Puzzle, Cmd a )
update msg model =
    case msg of
        KeyMsg keycode ->
            let
                d2 =
                    Debug.log "keycode" keycode
            in
                ( Puzzle.update (Key.fromKeyCode keycode) model, Cmd.none )

        FetchSucceed problems ->
            ( Puzzle.create problems, Cmd.none )

        FetchFail error ->
            let
                d =
                    Debug.log "Fetch failed:" error
            in
                ( model, Cmd.none )


subscriptions : Puzzle -> Sub Msg
subscriptions model =
    Keyboard.downs KeyMsg


view : Puzzle -> Html a
view model =
    case model of
        Puzzle.InProgress problems ->
            Html.div []
                [ View.board (Board.fromPuzzle model 50 69)
                , View.problemsRemaining (problems)
                , View.problem (Cons.head problems)
                ]

        Puzzle.Finished ->
            Html.text "You did it!"


getClues : Cmd Msg
getClues =
    let
        url =
            "http://gwil.co/projects/archeotype/dictionary.json"
    in
        Task.perform FetchFail FetchSucceed (Http.get Puzzle.fromJson url)


main : Program Never
main =
    Html.App.program
        { init = ( model, getClues )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
