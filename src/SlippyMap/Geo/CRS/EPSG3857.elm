module SlippyMap.Geo.CRS.EPSG3857 exposing (..)

{-| The most common CRS for online maps, used by almost all free and commercial tile providers. Uses Spherical Mercator projection.
-}

import SlippyMap.Geo.CRS exposing (CRS)
import SlippyMap.Geo.Location as Location exposing (Location)
import SlippyMap.Geo.Point as Point exposing (Point)
import SlippyMap.Geo.Projection.SphericalMercator as Projection
import SlippyMap.Geometry.Transformation as Transformation exposing (Transformation)


crs : CRS
crs =
    { locationToPoint = locationToPoint
    , pointToLocation = pointToLocation
    , project = project
    , unproject = unproject
    , scale = scale
    , zoom = zoom
    , distance = distance
    , code = code
    }


transformation : Transformation
transformation =
    let
        s =
            0.5 / (pi * Projection.radius)
    in
    Transformation s 0.5 -s 0.5


locationToPoint : Float -> Location -> Point
locationToPoint zoom location =
    location
        |> project
        |> Transformation.transform transformation


pointToLocation : Float -> Point -> Location
pointToLocation zoom point =
    point
        |> Transformation.untransform transformation
        |> unproject


project : Location -> Point
project =
    Projection.project


unproject : Point -> Location
unproject =
    Projection.unproject


scale : Float -> Float
scale zoom =
    256 * 2 ^ zoom


zoom : Float -> Float
zoom scale =
    logBase e (scale / 256) / logBase e 2


{-|

    distance
        { lon = -86.67, lat = 36.12 }
        { lon = -118.40, lat = 33.94 }
    --> 2886444.43

-}
distance : Location -> Location -> Float
distance from to =
    let
        radius =
            6371000

        deltaLon =
            degrees (to.lon - from.lon)

        deltaLat =
            degrees (to.lat - from.lat)

        a =
            (sin (deltaLat / 2) ^ 2)
                + (sin (deltaLon / 2) ^ 2)
                * cos (degrees from.lat)
                * cos (degrees to.lat)
    in
    radius * 2 * asin (sqrt a)


code : String
code =
    "EPSG:3857"