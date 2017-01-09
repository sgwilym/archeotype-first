module Main exposing (..)

import Html exposing (Html)
import Html.App
import Cons exposing (Cons, cons)
import Key exposing (Key)
import Letter exposing (Letter(..))
import Puzzle exposing (Puzzle)
import HiddenPicture exposing (HiddenPicture, Space(..))
import Keyboard exposing (KeyCode)
import View
import Board


type Msg
    = KeyMsg KeyCode


type alias Model =
    { currentGame : ( Puzzle, HiddenPicture ) }


canemPuzzle : Cons ( String, Cons Letter )
canemPuzzle =
    cons ( "Jumps high, bites small", cons F [ L, E, A, S ] )
        [ ( "Worn by shirts and hounds", cons C [ O, L, L, A, R ] )
        , ( "A healthy mix, looked down upon", cons M [ O, N, G, R, E, L ] )
        , ( "A fashionable game", cons F [ E, T, C, H ] )
        , ( "Often better than tree skin", cons B [ I, T, E ] )
        , ( "A job fit for man’s best friend", cons G [ U, A, R, D ] )
        , ( "Un_____ the beast", cons L [ E, A, S, H ] )
        , ( "Teeth and family", cons C [ A, N, I, N, E ] )
        , ( "The lingua franca of wolves and dogs", cons H [ O, W, L ] )
        , ( "Often worse than a bite", cons B [ A, R, K ] )
        ]


canemHiddenPicture : HiddenPicture
canemHiddenPicture =
    HiddenPicture.create
        "./pictures/canem.png"
        """
          # #
 #        #####
 #        ####
 ############
 ############
 ###      ###
 #         #
 #          #
 ##          ##
"""


model : Model
model =
    { currentGame = ( Puzzle.create canemPuzzle, canemHiddenPicture ) }


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        KeyMsg keycode ->
            ( { currentGame =
                    ( (Puzzle.update (Key.fromKeyCode keycode) (fst model.currentGame))
                    , (snd model.currentGame)
                    )
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyMsg


view : Model -> Html a
view model =
    case (fst model.currentGame) of
        Puzzle.InProgress problems solved ->
            Html.div []
                [ View.board (Board.create (fst model.currentGame) (snd model.currentGame) 69)
                , View.problemsRemaining (problems)
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
