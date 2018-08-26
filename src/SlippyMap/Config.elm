module SlippyMap.Config
    exposing
        ( Config
        , Interactions
        , attributionPrefixFrom
        , crsFrom
        , interactionsFrom
        , interactive
        , maxZoomFrom
        , minZoomFrom
        , pointerPositionDecoderFrom
        , sizeFrom
        , static
        , tagger
        , withAttributionPrefix
        , withCRS
        , withMaxZoom
        , withMinZoom
        , withZoomDelta
        , withZoomSnap
        , withoutAttributionControl
        , withoutZoomControl
        , zoomControlFrom
        , zoomDeltaFrom
        , zoomSnapFrom
        )

{-|

@docs Config, static, interactive, size, withCRS, withZoomSnap, withZoomDelta, withMaxZoom, withMinZoom, withoutZoomControl, zoomControl, Interactions, crs, minZoom, maxZoom, zoomDelta, zoomSnap, tagger, interactions, attributionPrefix, withoutAttributionControl, withAttributionPrefix, pointerPositionDecoder

-}

import Json.Decode as Decode exposing (Decoder)
import SlippyMap.Geo.CRS exposing (CRS)
import SlippyMap.Geo.CRS.EPSG3857 as EPSG3857
import SlippyMap.Geo.Point exposing (Point)
import SlippyMap.Msg exposing (Msg)
import DOM


{-| Configuration for the map.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { attributionPrefix : Maybe String
    , size : Point
    , minZoom : Float
    , maxZoom : Float
    , zoomSnap : Float
    , zoomDelta : Float
    , toMsg : Maybe (Msg -> msg)
    , crs : CRS
    , zoomControl : Bool
    , attributionControl : Bool
    , interactions : Interactions
    , pointerPositionDecoder : Decoder Point
    }


defaultConfigInternal : ConfigInternal msg
defaultConfigInternal =
    { attributionPrefix = Nothing
    , size = { x = 600, y = 400 }
    , minZoom = 0
    , maxZoom = 19
    , zoomSnap = 1
    , zoomDelta = 1
    , toMsg = Nothing
    , crs = EPSG3857.crs
    , zoomControl = False
    , attributionControl = True
    , interactions = interactiveInteractions
    , pointerPositionDecoder = domPointerPositionDecoder
    }


domPointerPositionDecoder : Decoder Point
domPointerPositionDecoder =
    Decode.map5
        (\clientX clientY rect clientLeft clientTop ->
            { x = clientX - rect.left - clientLeft
            , y = clientY - rect.top - clientTop
            }
        )
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        -- (DOM.target mapPosition)
        (Decode.field "currentTarget" mapPosition)
        (Decode.field "currentTarget" <|
            Decode.field "clientLeft" Decode.float
        )
        (Decode.field "currentTarget" <|
            Decode.field "clientTop" Decode.float
        )

mapPosition : Decoder DOM.Rectangle
mapPosition =
    Decode.oneOf
        [ DOM.boundingClientRect
        , Decode.lazy (\_ -> DOM.parentElement mapPosition)
        ]


{-| -}
type alias Interactions =
    { scrollWheelZoom : Bool
    , doubleClickZoom : Bool
    , touchZoom : Bool
    , keyboardControl : Bool
    }


interactiveInteractions : Interactions
interactiveInteractions =
    { scrollWheelZoom = True
    , doubleClickZoom = True
    , touchZoom = True
    , keyboardControl = True
    }


{-| -}
static : Point -> Config msg
static newSize =
    Config
        { defaultConfigInternal | size = newSize }


{-| -}
interactive : Point -> (Msg -> msg) -> Config msg
interactive newSize toMsg =
    Config
        { defaultConfigInternal
            | size = newSize
            , toMsg = Just toMsg
            , zoomControl = True
        }


{-| -}
withCRS : CRS -> Config msg -> Config msg
withCRS newCrs (Config configInternal) =
    Config
        { configInternal | crs = newCrs }


{-| -}
withoutZoomControl : Config msg -> Config msg
withoutZoomControl (Config configInternal) =
    Config
        { configInternal | zoomControl = False }


{-| -}
withAttributionPrefix : String -> Config msg -> Config msg
withAttributionPrefix prefix (Config configInternal) =
    Config
        { configInternal | attributionPrefix = Just prefix }


{-| -}
withoutAttributionControl : Config msg -> Config msg
withoutAttributionControl (Config configInternal) =
    Config
        { configInternal | attributionControl = False }


{-| -}
withMaxZoom : Float -> Config msg -> Config msg
withMaxZoom newMaxZoom (Config configInternal) =
    Config
        { configInternal | maxZoom = newMaxZoom }


{-| -}
withMinZoom : Float -> Config msg -> Config msg
withMinZoom newMinZoom (Config configInternal) =
    Config
        { configInternal | minZoom = newMinZoom }


{-| -}
withZoomSnap : Float -> Config msg -> Config msg
withZoomSnap newZoomSnap (Config configInternal) =
    Config
        { configInternal | zoomSnap = newZoomSnap }


{-| -}
withZoomDelta : Float -> Config msg -> Config msg
withZoomDelta newZoomDelta (Config configInternal) =
    Config
        { configInternal | zoomDelta = newZoomDelta }


{-| -}
sizeFrom : Config msg -> Point
sizeFrom (Config { size }) =
    size


{-| -}
crsFrom : Config msg -> CRS
crsFrom (Config { crs }) =
    crs


{-| -}
minZoomFrom : Config msg -> Float
minZoomFrom (Config { minZoom }) =
    minZoom


{-| -}
maxZoomFrom : Config msg -> Float
maxZoomFrom (Config { maxZoom }) =
    maxZoom


{-| -}
zoomDeltaFrom : Config msg -> Float
zoomDeltaFrom (Config { zoomDelta }) =
    zoomDelta


{-| -}
zoomSnapFrom : Config msg -> Float
zoomSnapFrom (Config { zoomSnap }) =
    zoomSnap


{-| -}
zoomControlFrom : Config msg -> Bool
zoomControlFrom (Config { zoomControl }) =
    zoomControl


{-| -}
tagger : Config msg -> Maybe (Msg -> msg)
tagger (Config { toMsg }) =
    toMsg


{-| -}
interactionsFrom : Config msg -> Interactions
interactionsFrom (Config { interactions }) =
    interactions


{-| -}
attributionPrefixFrom : Config msg -> Maybe String
attributionPrefixFrom (Config { attributionPrefix }) =
    attributionPrefix


{-| -}
pointerPositionDecoderFrom : Config msg -> Decoder Point
pointerPositionDecoderFrom (Config { pointerPositionDecoder }) =
    pointerPositionDecoder
