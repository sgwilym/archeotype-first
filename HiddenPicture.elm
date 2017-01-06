module HiddenPicture exposing (HiddenPicture, Grid(..), Space(..))

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
