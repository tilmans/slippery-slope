module SlippyMap.Layer.RemoteImage
    exposing
        ( Config
        , config
        , layer
        , toUrlFrom
        , withTile
        )

{-| A layer to display remote image tiles.

@docs Config, layer, toUrl, withTile, config

-}

import RemoteData exposing (WebData)
import SlippyMap.Geo.Tile exposing (Tile)
import SlippyMap.Layer exposing (Layer)
import SlippyMap.Layer.Tile as TileLayer
import SlippyMap.Map as Map exposing (Map)
import Svg exposing (Svg)
import Svg.Attributes


-- CONFIG


{-| Configuration for the layer.
-}
type Config
    = Config
        { toUrl : Tile -> String
        , fromTile : Tile -> WebData Tile
        }


{-| -}
config : String -> List String -> Config
config urlTemplate subDomains =
    Config
        { toUrl = TileLayer.toUrl urlTemplate subDomains
        , fromTile = always RemoteData.NotAsked
        }


{-| -}
withTile : (Tile -> WebData Tile) -> Config -> Config
withTile fromTile (Config configInternal) =
    Config
        { configInternal | fromTile = fromTile }


{-| -}
toUrlFrom : Config -> Tile -> String
toUrlFrom (Config { toUrl }) =
    toUrl



-- LAYER


{-| -}
layer : Config -> Layer msg
layer ((Config configInternal) as config_) =
    TileLayer.config configInternal.fromTile
        (tile config_)
        |> TileLayer.layer


tile : Config -> Map msg -> WebData Tile -> Svg msg
tile (Config configInternal) map tileResponse =
    let
        scale =
            Map.scaleZ map (toFloat (round <| Map.zoom map))
    in
    case tileResponse of
        RemoteData.NotAsked ->
            Svg.text_ [] [ Svg.text "Not Asked" ]

        RemoteData.Loading ->
            Svg.text_ [] [ Svg.text "Loading" ]

        RemoteData.Failure e ->
            Svg.text_ [] [ Svg.text ("Error: Can't load remote image") ] -- TODO:Better to string

        RemoteData.Success t ->
            Svg.image
                [ Svg.Attributes.width
                    -- (toString renderState.transform.tileSize)
                    "256"
                , Svg.Attributes.height
                    -- (toString renderState.transform.tileSize)
                    "256"
                , Svg.Attributes.xlinkHref (configInternal.toUrl t)
                , Svg.Attributes.transform
                    ("scale("
                        ++ String.fromFloat scale
                        ++ ")"
                    )
                ]
                []
