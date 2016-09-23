module Puzzle exposing (Puzzle, create, toProgress)

import Problem exposing (Problem, Letters)
import Cons exposing (Cons, cons)


-- TODO: Make puzzle a zip list


type alias Puzzle =
    { solvedProblems : List Problem
    , currentProblem : Problem
    , remainingProblems : List Problem
    }


type alias PuzzleLetters =
    { solvedLetters : Maybe Letters
    , remainingLetters : Letters
    }


type Progress
    = Continue Problem
    | Finish


create : Cons ( String, Letters ) -> Puzzle
create problems =
    let
        problems' =
            Cons.map
                (\( hint, solution ) -> Problem.create hint solution)
                problems
    in
        { solvedProblems = []
        , currentProblem = Cons.head problems'
        , remainingProblems = Cons.tail problems'
        }


toProgress : Puzzle -> Problem -> Problem.Result -> Progress
toProgress puzzle problem result =
    case result of
        Problem.Wrong ->
            Continue problem

        Problem.Correct letters ->
            Continue { problem | solution = letters }

        Problem.Solved ->
            case List.head puzzle.remainingProblems of
                Just puzzle ->
                    Continue puzzle

                Nothing ->
                    Finish


toPuzzleLetters : Puzzle -> PuzzleLetters
toPuzzleLetters { solvedProblems, currentProblem, remainingProblems } =
    { solvedLetters =
        List.map .solution solvedProblems
            |> List.map Cons.toList
            |> List.concat
            |> Cons.fromList
    , remainingLetters = Cons.concat (cons currentProblem.solution (List.map .solution remainingProblems))
    }
