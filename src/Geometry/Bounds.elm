module Geometry.Bounds
    exposing
        ( Bounds
        , extend
        , getCenter
        , getBottomLeft
        , getTopRight
        , getSize
        )

{-| # Bounds
@docs Bounds, extend, getCenter, getBottomLeft, getTopRight, getSize

-}

import Geometry.Point as Point exposing (Point, subtract)


{-| Represents a rectangular area in pixel coordinates. `min` represents
the top left corner of the rectangle and `max` represents the bottom right
corner of the rectangle.
-}
type alias Bounds =
    { min : Point
    , max : Point
    }


{-| Extends the bounds to contain the given point.
-}
extend : Bounds -> Point -> Bounds
extend bounds point =
    let
        minX =
            Basics.min bounds.min.x point.x

        minY =
            Basics.min bounds.min.y point.y

        maxX =
            Basics.max bounds.max.x point.x

        maxY =
            Basics.max bounds.max.y point.y
    in
        Bounds
            (Point minX minY)
            (Point maxX maxY)


{-| Returns the center point of the bounds.
-}
getCenter : Bool -> Bounds -> Point
getCenter rounding bounds =
    let
        x =
            ((bounds.min.x + bounds.max.x) / 2)

        y =
            ((bounds.min.y + bounds.max.y) / 2)
    in
        Point x y


{-| Returns the bottom-left point of the bounds.
-}
getBottomLeft : Bounds -> Point
getBottomLeft bounds =
    Point
        bounds.min.x
        bounds.max.y


{-| Returns the top-right point of the bounds.
-}
getTopRight : Bounds -> Point
getTopRight bounds =
    Point
        bounds.max.x
        bounds.min.y


{-| Returns the size of the given bounds
-}
getSize : Bounds -> Point
getSize bounds =
    Point.subtract bounds.max bounds.min
