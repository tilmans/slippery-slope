module SlippyMap.Layer.RemoteTile
    exposing
        ( Config
        , config
        , layer
        , toUrlFrom
        , withRender
        , withTile
        )

{-| A layer to display generic JSON tiles.

@docs Config, layer, toUrl, withTile, withRender, config

-}

import RemoteData exposing (WebData)
import SlippyMap.Geo.Tile exposing (Tile)
import SlippyMap.Layer exposing (Layer)
import SlippyMap.Layer.Tile as TileLayer
import SlippyMap.Map exposing (Map)
import Svg exposing (Svg)


-- CONFIG


{-| Configuration for the layer.
-}
type Config data msg
    = Config (ConfigInternal data msg)


type alias ConfigInternal data msg =
    { toUrl : Tile -> String
    , fromTile : Tile -> ( Tile, WebData data )
    , render : ( Tile, data ) -> Map msg -> Svg msg
    }


{-| -}
config : String -> List String -> Config data msg
config urlTemplate subDomains =
    Config
        { toUrl = TileLayer.toUrl urlTemplate subDomains
        , fromTile = \tile_ -> ( tile_, RemoteData.NotAsked )
        , render = \_ _ -> Svg.text ""
        }


{-| -}
withTile : (Tile -> ( Tile, WebData data )) -> Config data msg -> Config data msg
withTile fromTile (Config configInternal) =
    Config
        { configInternal | fromTile = fromTile }


{-| -}
withRender : (( Tile, data ) -> Map msg -> Svg msg) -> Config data msg -> Config data msg
withRender render (Config configInternal) =
    Config
        { configInternal | render = render }


{-| -}
toUrlFrom : Config data msg -> Tile -> String
toUrlFrom (Config { toUrl }) =
    toUrl



-- LAYER


{-| -}
layer : Config data msg -> Layer msg
layer ((Config configInternal) as config_) =
    TileLayer.config configInternal.fromTile
        (tile config_)
        |> TileLayer.layer


tile : Config data msg -> Map msg -> ( Tile, WebData data ) -> Svg msg
tile (Config configInternal) map ( tile_, tileResponse ) =
    case tileResponse of
        RemoteData.NotAsked ->
            Svg.text_ [] [ Svg.text "Not Asked" ]

        RemoteData.Loading ->
            Svg.text_ [] [ Svg.text "Loading" ]

        RemoteData.Failure e ->
            Svg.text_ [] [ Svg.text ("Error Loading Remote Data") ] -- TODO: Add better handling

        RemoteData.Success value ->
            configInternal.render ( tile_, value ) map
