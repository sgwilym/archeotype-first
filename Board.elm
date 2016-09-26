module Board exposing (Board, Cell(..), fromPuzzle)

import Cons exposing (Cons, cons)
import Puzzle exposing (Puzzle(..))
import Problem exposing (Problem)
import Attempt
import Letter exposing (Letter(..))
import Random
import String


type Cell
    = Cell Letter
    | EmptyCell


type alias Board =
    { cells : Cons Cell
    , width : Int
    , seed : Int
    }



-- BUG: the first letter disappears, even when the first letter is wrong!


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
                        case index < (Cons.length attemptLetters) of
                            True ->
                                EmptyCell

                            False ->
                                Cell letter
                    )
                    answer

        Nothing ->
            Cons.map (\letter -> Cell letter) answer


fromPuzzle : Puzzle -> Int -> Int -> Board
fromPuzzle puzzle width seed =
    case puzzle of
        InProgress problems ->
            let
                sortedProblems =
                    Cons.map fst
                        (Cons.sortBy snd
                            (Cons.map2
                                (,)
                                problems
                                (Cons.map
                                    (\problem ->
                                        String.fromList
                                            (Cons.toList
                                                (Cons.map Letter.toChar problem.answer)
                                            )
                                    )
                                    problems
                                )
                            )
                        )

                cells =
                    Cons.map cellsFromProblem sortedProblems

                flatCells =
                    Cons.concat cells

                seed' =
                    Random.initialSeed seed

                randomCons cellSeed =
                    Random.step
                        (Random.map
                            (\n -> cons n [])
                            (Random.int 0 100)
                        )
                        (Random.initialSeed
                            cellSeed
                        )

                randomValues =
                    Cons.concat
                        (Cons.indexedMap
                            ((\index _ ->
                                fst (randomCons (seed + index))
                             )
                            )
                            flatCells
                        )

                shuffledCells =
                    (Cons.map fst
                        (Cons.sortBy snd
                            (Cons.map2 (,) flatCells randomValues)
                        )
                    )
            in
                { cells = shuffledCells, width = width, seed = seed }

        Finished ->
            { cells = cons EmptyCell [], width = width, seed = seed }
