module SlippyMap.State
    exposing
        ( State
        , animate
        , around
        , at
        , defaultState
        , fitBounds
        , focusFrom
        , getScene
        , interactionFrom
        , moveBy
        , moveTo
        , setCenter
        , setFocus
        , setInteraction
        , setScene
        , setTransition
        , setZoom
        , snapZoom
        , tickTransition
        , transitionFrom
        , withInteraction
        , zoomByAround
        , zoomIn
        , zoomInAround
        , zoomOut
        )

{-|

@docs State, at, around, defaultState, fitBounds, moveBy, moveTo, setCenter, setFocus, setInteraction, setTransition, setScene, setZoom, tickTransition, withInteraction, zoomByAround, zoomIn, zoomInAround, zoomOut, getScene, interaction, focus, transition, snapZoom, animate

-}

import SlippyMap.Config as Config exposing (Config)
import SlippyMap.Geo.Location as Location exposing (Location)
import SlippyMap.Geo.Point as Point exposing (Point)
import SlippyMap.Transform as Transform
import SlippyMap.Types exposing (..)
import Time


{-| -}
type State
    = State
        { scene : Scene
        , transition : Transition
        , interaction : Interaction
        , focus : Focus
        }


{-| -}
defaultState : State
defaultState =
    State
        { scene = defaultScene
        , transition = NoTransition
        , interaction = NoInteraction
        , focus = HasNoFocus
        }


defaultScene : Scene
defaultScene =
    { center = Location 0 0
    , zoom = 0
    }



-- CONSTRUCTORS


{-| -}
at : Config msg -> Scene -> State
at _ initialScene =
    setScene initialScene defaultState


{-| -}
around : Config msg -> Location.Bounds -> State
around config initialBounds =
    fitBounds config initialBounds defaultState



-- BASE SETTERS


{-| -}
setScene : Scene -> State -> State
setScene newScene (State state) =
    State
        { state
            | scene = newScene
            , transition = NoTransition
        }


{-| -}
setTransition : Transition -> State -> State
setTransition newTransition (State state) =
    State
        { state | transition = newTransition }


{-| -}
setInteraction : Interaction -> State -> State
setInteraction newInteraction (State state) =
    State
        { state | interaction = newInteraction }


{-| -}
setFocus : Focus -> State -> State
setFocus newFocus (State state) =
    State
        { state | focus = newFocus }



-- INTERACTIONS


{-| -}
withInteraction : Config msg -> State -> State
withInteraction config ((State { interaction }) as state) =
    case interaction of
        Dragging drag ->
            withDrag config drag state

        Pinching pinch ->
            withPinch config pinch state

        NoInteraction ->
            state


withDrag : Config msg -> Drag -> State -> State
withDrag config { last, current } state =
    let
        size =
            Config.sizeFrom config

        newCenterPoint =
            size
                |> Point.divideBy 2
                |> Point.add
                    { x = toFloat (last.x - current.x)
                    , y = toFloat (last.y - current.y)
                    }
    in
    moveTo config newCenterPoint state


withPinch : Config msg -> Pinch -> State -> State
withPinch config { last, current } ((State { scene }) as state) =
    let
        ( crs, size ) =
            ( Config.crsFrom config, Config.sizeFrom config )

        toPoint position =
            { x = toFloat position.x
            , y = toFloat position.y
            }

        centerPoint ( pos1, pos2 ) =
            Point.center
                (toPoint pos1)
                (toPoint pos2)

        distance ( pos1, pos2 ) =
            Point.distance
                (toPoint pos1)
                (toPoint pos2)

        lastCenter =
            centerPoint last

        lastDistance =
            distance last

        currentCenter =
            centerPoint current

        currentDistance =
            distance current

        newCenterPoint =
            size
                |> Point.divideBy 2
                |> Point.add lastCenter
                |> Point.subtract currentCenter

        zoomDelta =
            crs.zoom (crs.scale 0 * currentDistance / lastDistance)
    in
    state
        |> moveTo config newCenterPoint
        -- |> zoomByAround config zoomDelta currentCenter
        |> setZoom config (scene.zoom + zoomDelta)



-- UPDATES


{-| -}
moveTo : Config msg -> Point -> State -> State
moveTo config newCenterPoint ((State { scene }) as state) =
    let
        ( crs, size ) =
            ( Config.crsFrom config
            , Config.sizeFrom config
            )

        transform =
            Transform.transformWith crs size scene

        newCenter =
            Transform.screenPointToLocation transform
                newCenterPoint
    in
    setCenter config newCenter state


{-| -}
moveBy : Config msg -> Point -> State -> State
moveBy config offset state =
    let
        size =
            Config.sizeFrom config

        newCenterPoint =
            size
                |> Point.divideBy 2
                |> Point.add offset
    in
    moveTo config newCenterPoint state


{-| -}
setCenter : Config msg -> Location -> State -> State
setCenter _ newCenter ((State { scene }) as state) =
    setScene
        { scene | center = newCenter }
        state


{-| -}
setZoom : Config msg -> Float -> State -> State
setZoom config newZoom ((State { scene }) as state) =
    if isNaN newZoom || isInfinite newZoom then
        state
    else
        let
            ( minZoom, maxZoom ) =
                ( Config.minZoomFrom config
                , Config.maxZoomFrom config
                )
        in
        setScene
            { scene | zoom = clamp minZoom maxZoom newZoom }
            state


{-| -}
snapZoom : Config msg -> State -> State
snapZoom config ((State { scene }) as state) =
    let
        ( zoomSnap, minZoom, maxZoom ) =
            ( Config.zoomSnapFrom config
            , Config.minZoomFrom config
            , Config.maxZoomFrom config
            )

        zoomSnapped =
            clamp minZoom maxZoom <|
                if zoomSnap == 0 then
                    scene.zoom
                else
                    toFloat (floor (scene.zoom / zoomSnap)) * zoomSnap
    in
    setZoom config zoomSnapped state


{-| -}
zoomIn : Config msg -> State -> State
zoomIn config ((State { scene }) as state) =
    setZoom config
        (scene.zoom + Config.zoomDeltaFrom config)
        state


{-| -}
zoomOut : Config msg -> State -> State
zoomOut config ((State { scene }) as state) =
    setZoom config
        (scene.zoom - Config.zoomDeltaFrom config)
        state


{-| -}
zoomInAround : Config msg -> Point -> State -> State
zoomInAround config =
    zoomByAround config (Config.zoomDeltaFrom config)


{-| -}
zoomByAround : Config msg -> Float -> Point -> State -> State
zoomByAround config delta around_ ((State { scene }) as state) =
    let
        crs = Config.crsFrom config
        size = Config.sizeFrom config
        minZoom = Config.minZoomFrom config
        maxZoom = Config.maxZoomFrom config

        newZoom =
            if isNaN delta || isInfinite delta then
                scene.zoom
            else
                clamp minZoom maxZoom (scene.zoom + delta)

        transform =
            Transform.transformWith crs size scene

        transformZoomed =
            Transform.transformWith crs size { scene | zoom = newZoom }

        currentCenterPoint =
            Transform.locationToPoint transform scene.center

        aroundPoint =
            around_
                |> Point.add currentCenterPoint
                |> Point.subtract (Point.divideBy 2 size)

        aroundLocation =
            Transform.pointToLocation transform aroundPoint

        aroundPointZoomed =
            Transform.locationToPoint transformZoomed aroundLocation

        aroundPointDiff =
            Point.subtract aroundPoint aroundPointZoomed

        newCenter =
            Transform.pointToLocation transformZoomed
                (Point.add currentCenterPoint aroundPointDiff)
    in
    setScene
        { center = newCenter
        , zoom = newZoom
        }
        state


{-| -}
fitBounds : Config msg -> Location.Bounds -> State -> State
fitBounds config { southWest, northEast } state =
    let
        crs = Config.crsFrom config
        size = Config.sizeFrom config
        zoomSnap = Config.zoomSnapFrom config
        minZoom = Config.minZoomFrom config
        maxZoom = Config.maxZoomFrom config

        transform =
            Transform.transformWith crs
                size
                -- scene
                { center = Location 0 0
                , zoom = 0
                }

        southWestPoint =
            Transform.locationToPoint transform southWest

        northEastPoint =
            Transform.locationToPoint transform northEast

        boundsSize =
            Point.subtract
                { x = southWestPoint.x, y = northEastPoint.y }
                { x = northEastPoint.x, y = southWestPoint.y }

        scale =
            min
                (size.x / boundsSize.x)
                (size.y / boundsSize.y)

        zoom =
            crs.zoom (crs.scale 0 * scale)

        zoomSnapped =
            clamp minZoom maxZoom <|
                if zoomSnap == 0 then
                    zoom
                else
                    toFloat (floor (zoom / zoomSnap)) * zoomSnap

        center =
            Point.center
                southWestPoint
                northEastPoint
                |> Transform.pointToLocation transform
    in
    setScene
        { center = center
        , zoom = zoomSnapped
        }
        state



-- BASE GETTER


{-| -}
getScene : State -> Scene
getScene (State { scene }) =
    scene


{-| -}
interactionFrom : State -> Interaction
interactionFrom (State { interaction }) =
    interaction


{-| -}
focusFrom : State -> Focus
focusFrom (State { focus }) =
    focus


{-| -}
transitionFrom : State -> Transition
transitionFrom (State { transition }) =
    transition



-- TRANSITION


{-| -}
animate : Float -> (State -> State) -> State -> State
animate duration update state =
    let
        transition =
            MoveTo
                { fromScene = getScene state
                , toScene = getScene (update state)
                , duration = duration
                , elapsed = 0
                }
    in
    setTransition transition state


{-| -}
tickTransition : Float -> State -> State
tickTransition diff ((State { transition, scene }) as state) =
    case transition of
        MoveTo target ->
            let
                fromScene =
                    target.fromScene

                toScene =
                    target.toScene

                -- NOTE: Use onAnimationFrame instead?
                newElapsed =
                    target.elapsed + diff

                targetDurationFloat = target.duration

                progress =
                    clamp 0 1 (newElapsed / targetDurationFloat)
                        |> (\time ->
                                1 - (1 - time) ^ 4
                           )

                newScene =
                    { scene
                        | zoom =
                            fromScene.zoom
                                + (toScene.zoom - fromScene.zoom)
                                * progress
                        , center =
                            { lon =
                                fromScene.center.lon
                                    + (toScene.center.lon - fromScene.center.lon)
                                    * progress
                            , lat =
                                fromScene.center.lat
                                    + (toScene.center.lat - fromScene.center.lat)
                                    * progress
                            }
                    }

                newTransition =
                    if progress >= 1 then
                        NoTransition
                    else
                        MoveTo
                            { target
                                | elapsed = newElapsed
                            }
            in
            state
                |> setScene newScene
                |> setTransition newTransition

        FlyTo target ->
            let
                fromScene =
                    target.fromScene

                toScene =
                    target.toScene

                newElapsed =
                    target.elapsed + diff

                targetDurationFloat = target.duration

                progress =
                    clamp 0 1 (newElapsed / targetDurationFloat)
                        |> (\time ->
                                1 - (1 - time) ^ 4
                           )

                newScene =
                    { scene
                        | zoom =
                            fromScene.zoom
                                + (toScene.zoom - fromScene.zoom)
                                * progress
                        , center =
                            Location.betweenAt fromScene.center toScene.center progress
                    }

                newTransition =
                    if progress >= 1 then
                        NoTransition
                    else
                        FlyTo
                            { target
                                | elapsed = newElapsed
                            }
            in
            state
                |> setScene newScene
                |> setTransition newTransition

        NoTransition ->
            state
