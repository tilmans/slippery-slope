module SlippyMap.GeoJson.Svg
    exposing
        ( Config
        , config
        , renderGeoJson
        , renderGeoJsonGeometry
        , renderGeoJsonLineString
        , renderGeoJsonPoint
        , renderGeoJsonPolygon
        , withAttributes
        , withPointRenderer
        )

{-| GeoJson SVG renderer.

@docs Config, config, withAttributes, withPointRenderer, renderGeoJson, renderGeoJsonPoint, renderGeoJsonLineString, renderGeoJsonPolygon, renderGeoJsonGeometry

-}

import GeoJson exposing (GeoJson)
import Json.Decode
import Json.Encode as Json
import SlippyMap.Geo.Point exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes


{-| -}
type Config msg
    = Config
        { project : GeoJson.Position -> Point
        , attributes : GeoJson.FeatureObject -> List (Svg.Attribute msg)
        , renderPoint : List (Svg.Attribute msg) -> Svg msg
        }


{-| -}
config : (GeoJson.Position -> Point) -> Config msg
config project =
    Config
        { project = project
        , attributes = defaultAttributes
        , renderPoint = circle
        }


{-| -}
withAttributes : (GeoJson.FeatureObject -> List (Svg.Attribute msg)) -> Config msg -> Config msg
withAttributes attributes (Config configuration) =
    Config { configuration | attributes = attributes }


{-| -}
withPointRenderer : (List (Svg.Attribute msg) -> Svg msg) -> Config msg -> Config msg
withPointRenderer render (Config configuration) =
    Config { configuration | renderPoint = render }


defaultAttributes : GeoJson.FeatureObject -> List (Svg.Attribute msg)
defaultAttributes _ =
    [ Svg.Attributes.stroke "#3388ff"
    , Svg.Attributes.strokeWidth "2"
    , Svg.Attributes.fill "#3388ff"
    , Svg.Attributes.fillOpacity "0.2"
    , Svg.Attributes.strokeLinecap "round"
    , Svg.Attributes.strokeLinejoin "round"
    ]


circle : List (Svg.Attribute msg) -> Svg msg
circle attributes =
    Svg.circle
        (Svg.Attributes.r "8" :: attributes)
        []


{-| -}
renderGeoJson : Config msg -> GeoJson -> Svg msg
renderGeoJson configuration ( geoJsonObject, _ ) =
    Svg.g []
        (renderGeoJsonObject configuration geoJsonObject)


{-| -}
renderGeoJsonObject : Config msg -> GeoJson.GeoJsonObject -> List (Svg msg)
renderGeoJsonObject configuration geoJsonObject =
    case geoJsonObject of
        GeoJson.Geometry geometry ->
            --renderGeoJsonGeometry configuration [] geometry
            renderGeoJsonFeatureObject configuration
                { id = Nothing
                , properties = Json.null
                , geometry = Just geometry
                }

        GeoJson.Feature featureObject ->
            renderGeoJsonFeatureObject configuration featureObject

        GeoJson.FeatureCollection featureCollection ->
            List.concatMap (renderGeoJsonFeatureObject configuration) featureCollection


{-| -}
renderGeoJsonFeatureObject : Config msg -> GeoJson.FeatureObject -> List (Svg msg)
renderGeoJsonFeatureObject ((Config { attributes }) as configuration) featureObject =
    Maybe.map (renderGeoJsonGeometry configuration (attributes featureObject ++ propertiesStyle featureObject))
        featureObject.geometry
        |> Maybe.withDefault []


{-| -}
renderGeoJsonGeometry : Config msg -> List (Svg.Attribute msg) -> GeoJson.Geometry -> List (Svg msg)
renderGeoJsonGeometry configuration attributes geometry =
    case geometry of
        GeoJson.Point position ->
            renderGeoJsonPoint configuration attributes position

        GeoJson.MultiPoint positionList ->
            List.concatMap (renderGeoJsonPoint configuration attributes) positionList

        GeoJson.LineString positionList ->
            renderGeoJsonLineString configuration attributes positionList

        GeoJson.MultiLineString positionListList ->
            renderGeoJsonMultiLineString configuration attributes positionListList

        GeoJson.Polygon positionListList ->
            renderGeoJsonPolygon configuration attributes positionListList

        GeoJson.MultiPolygon positionListListList ->
            List.concatMap (renderGeoJsonPolygon configuration attributes) positionListListList

        GeoJson.GeometryCollection geometryList ->
            List.concatMap (renderGeoJsonGeometry configuration attributes) geometryList


{-| -}
renderGeoJsonPoint : Config msg -> List (Svg.Attribute msg) -> GeoJson.Position -> List (Svg msg)
renderGeoJsonPoint (Config internalConfig) attributes position =
    let
        { x, y } =
            internalConfig.project position
    in
    [ Svg.g
        [ Svg.Attributes.transform
            ("translate("
                ++ String.fromFloat x
                ++ " "
                ++ String.fromFloat y
                ++ ")"
            )
        ]
        [ internalConfig.renderPoint attributes ]
    ]


{-| -}
renderGeoJsonLineString : Config msg -> List (Svg.Attribute msg) -> List GeoJson.Position -> List (Svg msg)
renderGeoJsonLineString configuration attributes positionList =
    [ Svg.path
        (attributes
            ++ [ Svg.Attributes.fill "none"
               , pathPoints configuration positionList
                    |> Svg.Attributes.d
               ]
        )
        []
    ]


{-| -}
renderGeoJsonMultiLineString : Config msg -> List (Svg.Attribute msg) -> List (List GeoJson.Position) -> List (Svg msg)
renderGeoJsonMultiLineString configuration attributes positionListList =
    [ Svg.path
        (attributes
            ++ [ Svg.Attributes.fill "none"
               , positionListList
                    |> List.map (\positionList -> pathPoints configuration positionList)
                    |> String.join ""
                    |> Svg.Attributes.d
               ]
        )
        []
    ]


{-| -}
renderGeoJsonPolygon : Config msg -> List (Svg.Attribute msg) -> List (List GeoJson.Position) -> List (Svg msg)
renderGeoJsonPolygon configuration attributes positionListList =
    let
        pathDefinition =
            (positionListList
                |> List.map (pathPoints configuration)
                |> String.join ""
            )
                ++ "Z"
    in
    [ Svg.path
        (attributes
            ++ [ Svg.Attributes.fillRule "evenodd", Svg.Attributes.d pathDefinition ]
        )
        []
    ]


{-| -}
pathPoints : Config msg -> List GeoJson.Position -> String
pathPoints (Config internalConfig) positionList =
    positionList
        |> List.map
            (\position ->
                internalConfig.project position
                    |> (\{ x, y } -> String.fromFloat x ++ "," ++ String.fromFloat y)
            )
        |> String.join "L"
        |> (\ll -> "M" ++ ll)


propertiesStyle : GeoJson.FeatureObject -> List (Svg.Attribute msg)
propertiesStyle { properties } =
    let
        decodedProperties =
            Json.Decode.decodeValue propertiesDecoder properties
    in
    case decodedProperties of
        Ok props ->
            [ -- Hm, title attributes won't work in SVG in general
              Maybe.map Svg.Attributes.title props.title
            , Maybe.map Svg.Attributes.stroke props.stroke
            , Maybe.map (String.fromFloat >> Svg.Attributes.strokeOpacity) props.strokeOpacity
            , Maybe.map (String.fromFloat >> Svg.Attributes.strokeWidth) props.strokeWidth
            , Maybe.map Svg.Attributes.fill props.fill
            , Maybe.map (String.fromFloat >> Svg.Attributes.fillOpacity) props.fillOpacity
            ]
                |> List.filterMap identity

        Err _ ->
            []


{-| <https://github.com/mapbox/simplestyle-spec/tree/master/1.1.0>
-}
type alias Properties =
    { title : Maybe String
    , markerColor : Maybe String
    , markerSize : Maybe MarkerSize
    , stroke : Maybe String
    , strokeOpacity : Maybe Float
    , strokeWidth : Maybe Float
    , fill : Maybe String
    , fillOpacity : Maybe Float
    }


type MarkerSize
    = MarkerSizeSmall
    | MarkerSizeMedium
    | MarkerSizeLarge


propertiesDecoder : Json.Decode.Decoder Properties
propertiesDecoder =
    Json.Decode.map8 Properties
        (Json.Decode.maybe <|
            Json.Decode.field "title"
                Json.Decode.string
        )
        (Json.Decode.maybe <|
            Json.Decode.field "marker-color"
                Json.Decode.string
        )
        (Json.Decode.maybe <|
            (Json.Decode.field "marker-size"
                Json.Decode.string
                |> Json.Decode.andThen markerSizeDecoder
            )
        )
        (Json.Decode.maybe <|
            Json.Decode.field "stroke"
                Json.Decode.string
        )
        (Json.Decode.maybe <|
            Json.Decode.field "stroke-opacity"
                Json.Decode.float
        )
        (Json.Decode.maybe <|
            Json.Decode.field "stroke-width"
                Json.Decode.float
        )
        (Json.Decode.maybe <|
            Json.Decode.field "fill"
                Json.Decode.string
        )
        (Json.Decode.maybe <|
            Json.Decode.field "fill-opacity"
                Json.Decode.float
        )


markerSizeDecoder : String -> Json.Decode.Decoder MarkerSize
markerSizeDecoder size =
    Json.Decode.succeed <|
        case size of
            "small" ->
                MarkerSizeSmall

            "medium" ->
                MarkerSizeMedium

            "large" ->
                MarkerSizeLarge

            _ ->
                MarkerSizeMedium
