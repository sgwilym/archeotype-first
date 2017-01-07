module Board exposing (Board, Cell(..), create)

import Cons exposing (Cons, cons)
import Puzzle exposing (Puzzle(..))
import HiddenPicture exposing (HiddenPicture, Space(..), Grid(..))
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
    , seed : Int
    , width : Int
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


zipSpacesWithCells : Cons Space -> Cons Cell -> Cons Cell
zipSpacesWithCells spaces cells =
    let
        -- if the head of spaces is open, then the returned head should be a letter
        -- if it's closed, the head should be an empty cell
        head =
            case Cons.head spaces of
                Open ->
                    Cons.head cells

                Closed ->
                    EmptyCell

        tail =
            -- if the head is a cell, then we want to get more cells
            case head of
                Cell _ ->
                    -- if there are more cells, and more spaces, we want to zip those up
                    case Cons.tail' cells of
                        Just moreCells ->
                            case Cons.tail' spaces of
                                Just moreSpaces ->
                                    Cons.toList (zipSpacesWithCells moreSpaces moreCells)

                                Nothing ->
                                    []

                        Nothing ->
                            []

                -- if the cell is empty, we need to figure out if we should pass ALL of the cells
                -- on the the next call, or just the tail
                EmptyCell ->
                    case Cons.head spaces of
                        Closed ->
                            -- This is definitely emptycell because of a blocked space
                            case Cons.tail' spaces of
                                Just moreSpaces ->
                                    case Cons.tail' cells of
                                        Just moreCells ->
                                            Cons.toList (zipSpacesWithCells moreSpaces cells)

                                        Nothing ->
                                            []

                                Nothing ->
                                    []

                        Open ->
                            -- This is definitely emptycell because of the player
                            case Cons.tail' spaces of
                                Just moreSpaces ->
                                    case Cons.tail' cells of
                                        Just moreCells ->
                                            Cons.toList (zipSpacesWithCells moreSpaces moreCells)

                                        Nothing ->
                                            []

                                Nothing ->
                                    []
    in
        Cons.cons head tail


create : Puzzle -> HiddenPicture -> Int -> Board
create puzzle hiddenPicture seed =
    case puzzle of
        InProgress problems solved ->
            let
                allProblems =
                    Cons.appendList problems solved

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
                    Cons.map problemToChars allProblems

                -- [EmptyCell, EmptyCell, Cell P, Cell P, Cell L, etc... ]
                problemCells =
                    Cons.map cellsFromProblem allProblems

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

                -- zip the picture’s spaces with the shuffled cells
                ( spaces, width ) =
                    case hiddenPicture.grid of
                        Grid spaces width ->
                            ( spaces, width )

                shuffledArrangedCells =
                    zipSpacesWithCells spaces shuffledCells
            in
                { cells = shuffledArrangedCells, width = width, seed = seed }

        Finished ->
            { cells = cons EmptyCell [], width = 0, seed = seed }
