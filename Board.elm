module Board exposing (..)

import Cons exposing (Cons, cons)
import Puzzle exposing (Puzzle(..))
import Problem exposing (Problem)
import Attempt
import Key exposing (Letter(..))
import Random


type Cell
    = Cell Letter
    | EmptyCell


type alias Board =
    { cells : Cons Cell
    , width : Int
    , seed : Int
    }


cellsFromProblem : Problem -> Cons Cell
cellsFromProblem { answer, attempt } =
    case attempt of
        Just attempt' ->
            let
                attemptLetters =
                    case Attempt.last attempt' of
                        Attempt.Failure _ ->
                            let
                                allLetters =
                                    Attempt.toLetters attempt'
                            in
                                cons
                                    (Cons.head allLetters)
                                    (List.take
                                        (List.length (Cons.tail allLetters) - 1)
                                        (Cons.tail allLetters)
                                    )

                        Attempt.Success _ _ ->
                            Attempt.toLetters attempt'

                        Attempt.Complete _ ->
                            Attempt.toLetters attempt'

                        Attempt.Incomplete _ ->
                            Attempt.toLetters attempt'
            in
                Cons.indexedMap
                    (\index letter ->
                        case index < (Cons.length attemptLetters) - 1 of
                            True ->
                                Cell letter

                            False ->
                                EmptyCell
                    )
                    answer

        Nothing ->
            Cons.map (\letter -> Cell letter) answer


fromPuzzle : Puzzle -> Int -> Int -> Board
fromPuzzle puzzle width seed =
    case puzzle of
        InProgress problems ->
            let
                cells =
                    Cons.map cellsFromProblem problems

                flatCells =
                    Cons.concat cells

                seed' =
                    Random.initialSeed seed

                randomValues =
                    Random.step
                        (Random.map
                            (\n -> cons n [])
                            (Random.int 0 100)
                        )
                        seed'

                shuffledCells =
                    (Cons.map fst
                        (Cons.sortBy snd
                            (Cons.map2 (,) flatCells (fst randomValues))
                        )
                    )
            in
                { cells = shuffledCells, width = width, seed = seed }

        Finished ->
            { cells = cons EmptyCell [], width = width, seed = seed }
