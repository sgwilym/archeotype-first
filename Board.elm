module Board exposing (Board, Cell(..), fromPuzzle)

import Cons exposing (Cons, cons)
import Puzzle exposing (Puzzle(..))
import Problem exposing (Problem)
import Attempt
import Letter exposing (Letter(..))
import Random
import String
import Char


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
                                    [ head ] ->
                                        Nothing

                                    head :: rest ->
                                        Just
                                            (cons
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
                problemToChars =
                    \problem ->
                        Cons.map Letter.toChar problem.answer

                randomCons consSeed =
                    Random.step
                        (Random.map
                            (\n -> cons n [])
                            (Random.int 0 100)
                        )
                        (Random.initialSeed consSeed)

                -- [['C', A, T], [D, O, G], [A, P, P, L, E]]
                problemChars =
                    Cons.map problemToChars problems

                -- [EmptyCell, EmptyCell, Cell P, Cell P, Cell L, etc... ]
                problemCells =
                    Cons.map cellsFromProblem problems

                -- (['A', 'P', 'P', 'L', 'E'] , [EmptyCell, EmptyCell, Cell P, Cell L, Cell E])
                ( sortedChars, sortedCells ) =
                    Cons.map2 (,) problemChars problemCells
                        |> Cons.sortBy (\( chars, _ ) -> String.fromList (Cons.toList chars))
                        |> Cons.unzip

                flatChars =
                    Cons.concat sortedChars

                flatCells =
                    Cons.concat sortedCells

                -- [12, 34, 54, 1, 4, 19, 1, 2, 100, 21, 11]
                randomValues =
                    Cons.indexedMap
                        (\index char ->
                            randomCons (index * Char.toCode char)
                        )
                        flatChars
                        |> Cons.map fst
                        |> Cons.concat

                -- Sort the list of cells by the random values
                shuffledCells =
                    Cons.map2 (,) flatCells randomValues
                        |> Cons.sortBy snd
                        |> Cons.map fst
            in
                { cells = shuffledCells, width = width, seed = seed }

        Finished ->
            { cells = cons EmptyCell [], width = width, seed = seed }
