module SlippyMap.Types
    exposing
        ( Drag
        , Focus(..)
        , Interaction(..)
        , Pinch
        , Scene
        , Size
        , Transition(..)
        , Position
        )

{-|

@docs Drag, Focus, Interaction, Pinch, Scene, Size, Transition

-}

import SlippyMap.Geo.Location exposing (Location)
import Time exposing (Posix)

type alias Position =
    { x: Int
    , y: Int
    }

{-| -}
type alias Size =
    { width : Int
    , height : Int
    }


{-| TODO: Rename to Camera, Detail, Sector, View, Clipping or something similar
-}
type alias Scene =
    { center : Location
    , zoom : Float
    }


{-| -}
type Transition
    = NoTransition
    | MoveTo
        { fromScene : Scene
        , toScene : Scene
        , duration : Float
        , elapsed : Float
        }
    | FlyTo
        { fromScene : Scene
        , toScene : Scene
        , duration : Float
        , elapsed : Float
        }


{-| Supported interactions
-}
type Interaction
    = NoInteraction
    | Dragging Drag
    | Pinching Pinch


{-| -}
type alias Drag =
    { last : Position
    , current : Position
    }


{-| -}
type alias Pinch =
    { last : ( Position, Position )
    , current : ( Position, Position )
    }


{-| -}
type Focus
    = HasFocus
    | HasNoFocus
