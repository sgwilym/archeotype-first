module Board exposing (..)

import Cons exposing (Cons, cons)
import Puzzle exposing (PuzzleLetters)


type Cell
    = Cell Char
    | EmptyCell


type alias Grid =
    { cells : Cons Cell
    , width : Int
    }


gridFromChars : Maybe Grid -> Letters -> Int -> Grid
gridFromChars grid letters width =
    case grid of
        Just grid ->
            { cells = emptyCells grid.cells letters
            , width = width
            }

        Nothing ->
            { cells = Cons.map (\letter -> Cell letter) letters
            , width = width
            }


emptyCells : Cons Cell -> PuzzleLetters -> Cons Cell
emptyCells cells puzzleLetters =
    cells



-- Map over the cells letters
-- If empty, skip
-- If a cell, check whether that letter is in new letters
-- if it is, retain the value
-- dont let that member be compared to the next cell somehow
-- if it's not, then change it to an empty cell
