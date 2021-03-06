module SlippyMap.Layer.Graticule
    exposing
        ( graticule
        , layer
        )

{-| A layer to display graticlues.

@docs layer, graticule

-}

import GeoJson exposing (GeoJson)
import Json.Encode as Json
import SlippyMap.Layer exposing (Layer)
import SlippyMap.Layer.GeoJson as GeoJsonLayer
import Svg
import Svg.Attributes


{-| -}
layer : Layer msg
layer =
    GeoJsonLayer.layer
        (GeoJsonLayer.styleConfig style)
        graticule


style : GeoJson.FeatureObject -> List (Svg.Attribute msg)
style _ =
    [ Svg.Attributes.stroke "#666"
    , Svg.Attributes.strokeWidth "0.5"
    , Svg.Attributes.strokeOpacity "0.5"
    , Svg.Attributes.fill "none"
    ]


{-| -}
graticule : GeoJson
graticule =
    ( GeoJson.FeatureCollection (lons ++ lats)
    , Nothing
    )


lats : List GeoJson.FeatureObject
lats =
    List.range -18 18
        |> List.map toFloat
        |> List.map
            (\lon ->
                List.range -80 80
                    |> List.map toFloat
                    |> List.map
                        (\lat ->
                            ( lon * 10 - 0.001, lat, 0 )
                        )
            )
        |> List.map
            (\points ->
                { id = Nothing
                , properties =
                    Json.object
                        [ ( "title", Json.string "Some lat" )
                        ]
                , geometry =
                    Just
                        (GeoJson.LineString points)
                }
            )


lons : List GeoJson.FeatureObject
lons =
    List.range -8 8
        |> List.map toFloat
        |> List.map
            (\lat ->
                List.range -180 180
                    |> List.map toFloat
                    |> List.map
                        (\lon ->
                            ( lon - 0.001, lat * 10, 0 )
                        )
            )
        |> List.map
            (\points ->
                { id = Nothing
                , properties =
                    Json.object
                        [ ( "title", Json.string "Some lon" ) ]
                , geometry =
                    Just
                        (GeoJson.LineString points)
                }
            )
