module SlippyMap.Map
    exposing
        ( Map
        , bounds
        , center
        , configFrom
        , crs
        , locationBounds
        , locationToPoint
        , locationToPointRelativeTo
        , locationToScreenPoint
        , make
        , origin
        , pointToLocation
        , scaleT
        , scaleZ
        , screenPointToLocation
        , size
        , stateFrom
        , tileCover
        , zoom
        )

{-|

@docs Map, make, bounds, locationBounds, center, crs, locationToPoint, locationToPointRelativeTo, locationToScreenPoint, origin, pointToLocation, scaleT, scaleZ, screenPointToLocation, size, tileCover, zoom, config, state

-}

import SlippyMap.Config as Config exposing (Config)
import SlippyMap.Geo.CRS exposing (CRS)
import SlippyMap.Geo.Location as Location exposing (Location)
import SlippyMap.Geo.Point as Point exposing (Point)
import SlippyMap.Geo.Tile exposing (Tile)
import SlippyMap.State as State exposing (State)
import SlippyMap.Transform as Transform exposing (Transformer)


{-| -}
type Map msg
    = Map
        { config : Config msg
        , state : State
        , transformer : Transformer
        }


{-| -}
make : Config msg -> State -> Map msg
make config state =
    let
        ( crs_, size_, scene ) =
            ( Config.crsFrom config
            , Config.sizeFrom config
            , State.getScene state
            )

        transformer =
            Transform.transformer crs_ size_ scene
    in
    Map
        { config = config
        , state = state
        , transformer = transformer
        }


{-| -}
size : Map msg -> Point
size =
    configFrom >> Config.sizeFrom


{-| -}
crs : Map msg -> CRS
crs =
    configFrom >> Config.crsFrom


{-| -}
configFrom : Map msg -> Config msg
configFrom (Map { config }) =
    config


{-| -}
stateFrom : Map msg -> State
stateFrom (Map { state }) =
    state


{-| -}
center : Map msg -> Location
center =
    stateFrom >> State.getScene >> .center


{-| -}
zoom : Map msg -> Float
zoom =
    stateFrom >> State.getScene >> .zoom


{-| -}
origin : Map msg -> Point
origin (Map { transformer }) =
    transformer.origin


{-| -}
bounds : Map msg -> Point.Bounds
bounds (Map { transformer }) =
    transformer.bounds


{-| -}
locationBounds : Map msg -> Location.Bounds
locationBounds (Map { transformer }) =
    transformer.locationBounds


{-| -}
scaleT : Map msg -> Float -> Float
scaleT (Map { transformer }) =
    transformer.scaleT


{-| -}
scaleZ : Map msg -> Float -> Float
scaleZ (Map { transformer }) =
    transformer.scaleZ


{-| -}
locationToPoint : Map msg -> Location -> Point
locationToPoint (Map { transformer }) =
    transformer.locationToPoint


{-| -}
locationToPointRelativeTo : Map msg -> Point -> Location -> Point
locationToPointRelativeTo (Map { transformer }) =
    transformer.locationToPointRelativeTo


{-| -}
locationToScreenPoint : Map msg -> Location -> Point
locationToScreenPoint (Map { transformer }) =
    transformer.locationToScreenPoint


{-| -}
pointToLocation : Map msg -> Point -> Location
pointToLocation (Map { transformer }) =
    transformer.pointToLocation


{-| -}
screenPointToLocation : Map msg -> Point -> Location
screenPointToLocation (Map { transformer }) =
    transformer.screenPointToLocation


{-| -}
tileCover : Map msg -> List Tile
tileCover (Map { transformer }) =
    transformer.tileCover
