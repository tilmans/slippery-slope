module SlippyMap.Subscriptions exposing (subscriptions)

{-|

@docs subscriptions

-}

import SlippyMap.Config as Config exposing (Config(..))
import SlippyMap.Msg exposing (DragMsg(..), Msg(..))
import SlippyMap.State as State exposing (State(..))
import SlippyMap.Types exposing (Focus(..), Interaction(..), Transition(..))
import Browser.Events exposing (onAnimationFrameDelta)


{-| -}
subscriptions : Config msg -> State -> Sub msg
subscriptions config state =
    case Config.tagger config of
        Just toMsg ->
            let
                ( interaction, focus, transition ) =
                    ( State.interactionFrom state
                    , State.focusFrom state
                    , State.transitionFrom state
                    )

                dragSubscriptions =
                    case interaction of
                        NoInteraction ->
                            []

                        Pinching _ ->
                            []

                        Dragging _ ->
                            []
                            -- TODO Port this
                            -- [ Mouse.moves (DragAt >> DragMsg)
                            -- , Mouse.ups (DragEnd >> DragMsg)
                            -- ]

                keyboardNavigationSubscriptions =
                    case focus of
                        HasFocus ->
                            []
                            -- TODO: Port this
                            -- [ Browse.Events.onKeyDown KeyboardNavigation ]

                        HasNoFocus ->
                            []

                transitionSubscriptions =
                    case transition of
                        NoTransition ->
                            []

                        MoveTo _ ->
                            [ onAnimationFrameDelta Tick ]

                        FlyTo _ ->
                            [ onAnimationFrameDelta Tick ]
            in
            (dragSubscriptions
                ++ keyboardNavigationSubscriptions
                ++ transitionSubscriptions
            )
                |> List.map (Sub.map toMsg)
                |> Sub.batch

        Nothing ->
            Sub.none
