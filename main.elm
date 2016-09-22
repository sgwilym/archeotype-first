module Main exposing (..)

import String


type alias Guess =
    Char


type alias Letters =
    List Char


type alias Puzzle =
    List Problem


type alias Problem =
    { hint : String, solution : Letters }


type Result
    = Wrong
    | Correct Letters
    | Solved


type Progress
    = Continue Problem
    | Finish


createPuzzle : List ( String, String ) -> Puzzle
createPuzzle problems =
    List.map
        (\( hint, solution ) -> createProblem hint solution)
        problems


createProblem : String -> String -> Problem
createProblem hint solution =
    { hint = hint
    , solution = String.toList solution
    }


toResult : Problem -> Guess -> Result
toResult puzzle guess =
    case List.head puzzle.solution of
        Just next ->
            if next == guess then
                case List.tail puzzle.solution of
                    Just tail ->
                        Correct tail

                    Nothing ->
                        Solved
            else
                Wrong

        Nothing ->
            Solved


toProgress : Puzzle -> Problem -> Result -> Progress
toProgress stage puzzle result =
    case result of
        Wrong ->
            Continue puzzle

        Correct letters ->
            Continue { puzzle | solution = letters }

        Solved ->
            case List.tail stage of
                Just tail ->
                    case List.head tail of
                        Just puzzle ->
                            Continue puzzle

                        Nothing ->
                            Finish

                Nothing ->
                    Finish
