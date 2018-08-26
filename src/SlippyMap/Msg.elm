module SlippyMap.Msg
    exposing
        ( DragMsg(..)
        , Msg(..)
        , PinchMsg(..)
        )

{-| Messages for map state updates.

@docs Msg, DragMsg, PinchMsg

-}

import SlippyMap.Geo.Point exposing (Point)
import SlippyMap.Types exposing (Focus, Position)
import Time exposing (Posix)

{-| -}
type Msg
    = ZoomIn
    | ZoomOut
    | ZoomInAround Point
    | ZoomByAround Float Point
    | DragMsg DragMsg
    | PinchMsg PinchMsg
    | SetFocus Focus
    | KeyboardNavigation String -- TODO: This will need a type
    | Tick Float


{-| -}
type DragMsg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


{-| -}
type PinchMsg
    = PinchStart ( Position, Position )
    | PinchAt ( Position, Position )
    | PinchEnd ( Position, Position )
