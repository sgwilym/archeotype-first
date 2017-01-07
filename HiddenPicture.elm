module HiddenPicture exposing (HiddenPicture, create, Grid(..), Space(..))

import String
import Cons exposing (Cons)


type alias HiddenPicture =
    { source : String
    , grid : Grid
    }


type Grid
    = Grid (Cons Space) Int


type Space
    = Open
    | Closed


create : String -> String -> HiddenPicture
create source gridString =
    let
        chars =
            case Cons.fromList (String.toList gridString) of
                Just chars ->
                    chars

                Nothing ->
                    Cons.cons ' ' []

        charsAsRowsOfSpaces =
            Cons.foldl
                (\char rows ->
                    case char of
                        '\n' ->
                            Cons.cons [] (Cons.toList rows)

                        '\t' ->
                            rows

                        ' ' ->
                            Cons.cons
                                (List.append (Cons.head rows) [ Closed ])
                                (Cons.tail rows)

                        _ ->
                            Cons.cons
                                (List.append (Cons.head rows) [ Open ])
                                (Cons.tail rows)
                )
                (Cons.cons [] [])
                chars
                |> Cons.reverse

        longestRow =
            Cons.foldr
                (\row maxLength ->
                    case List.length row > maxLength of
                        True ->
                            List.length row

                        False ->
                            maxLength
                )
                0
                charsAsRowsOfSpaces

        equalisedRows =
            Cons.map
                (\row ->
                    case Cons.fromList row of
                        Just spaces ->
                            case Cons.length spaces < longestRow of
                                True ->
                                    Cons.appendList spaces (List.repeat (longestRow - Cons.length spaces) Closed)

                                False ->
                                    spaces

                        Nothing ->
                            Cons.cons Closed (List.repeat (longestRow - 1) Closed)
                )
                charsAsRowsOfSpaces

        rowsAsSpaces =
            Cons.concat equalisedRows
    in
        { source = source, grid = Grid rowsAsSpaces longestRow }
