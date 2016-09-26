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
                                  case Cons.toList allLetters of
                                    [head] ->
                                       Nothing

                                    head::rest ->
                                      Just (
                                      cons
                                          head
                                          (List.take
                                              ((List.length rest) - 1)
                                              rest
                                          )
                                          )

                                    [] ->
                                      Nothing


                        Attempt.Success _ _ ->
                            Just (Attempt.toLetters attempt')

                        Attempt.Complete _ ->
                            Just (Attempt.toLetters attempt')

                        Attempt.Incomplete _ ->
                            Just (Attempt.toLetters attempt')
            in
                case attemptLetters of
                  Just letters ->
                    Cons.indexedMap
                        (\index letter ->
                            case index < (Cons.length letters) of
                                True ->
                                    EmptyCell

                                False ->
                                    Cell letter
                        )
                        answer

                  Nothing ->
                    Cons.map (\letter -> Cell letter) answer

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
