module SlippyMap.Map.Subscriptions exposing (subscriptions)

{-|
@docs subscriptions
-}

import AnimationFrame
import Keyboard exposing (KeyCode)
import Mouse exposing (Position)
import SlippyMap.Map.Config as Config exposing (Config(..))
import SlippyMap.Map.State as State exposing (State(..), Interaction(..), Drag, Focus(..))
import SlippyMap.Map.Msg as Msg exposing (Msg(..), DragMsg(..))


{-| -}
subscriptions : Config msg -> State -> Sub msg
subscriptions (Config config) ((State { interaction, focus, target }) as state) =
    case config.toMsg of
        Just toMsg ->
            let
                dragSubscriptions =
                    case interaction of
                        Nothing ->
                            []

                        Just (Pinching _) ->
                            []

                        Just (Dragging _) ->
                            [ Mouse.moves (DragAt >> DragMsg)
                            , Mouse.ups (DragEnd >> DragMsg)
                            ]

                keyboardNavigationSubscriptions =
                    case focus of
                        HasFocus ->
                            [ Keyboard.downs KeyboardNavigation ]

                        HasNoFocus ->
                            []

                stepSubscriptions =
                    case target of
                        Nothing ->
                            []

                        Just _ ->
                            [ AnimationFrame.diffs Step ]
            in
                (dragSubscriptions
                    ++ keyboardNavigationSubscriptions
                    ++ stepSubscriptions
                )
                    |> List.map (Sub.map toMsg)
                    |> Sub.batch

        Nothing ->
            Sub.none
