module Geometry.Point
    exposing
        ( Point
        , add
        , subtract
        , divideBy
        , multiplyBy
        , scaleBy
        , unscaleBy
        , round
        , floor
        , ceil
        , distanceTo
        , equals
        , contains
        , toString
        )

{-| # Point
@docs Point

# Point Mathematics
@docs add, subtract, divideBy, multiplyBy, scaleBy,
      unscaleBy, distanceTo

# Rounding
@docs round, floor, ceil

# Comparison
@docs equals, contains

# Conversion
@docs toString
-}

import FormatNumber exposing (Locale, formatFloat)


{-| Represents a point with `x` and `y` coordinates in pixels.
-}
type alias Point =
    { x : Float
    , y : Float
    }


{-| Returns the result of addition of the given points.

    add (Point 50 30) (Point 30 20) == Point 70 50
-}
add : Point -> Point -> Point
add a b =
    Point
        (a.x + b.x)
        (a.y + b.y)


{-| Returns the result of subtraction of the given points.

    subtract (Point 50 30) (Point 20 10) == Point 30 20
-}
subtract : Point -> Point -> Point
subtract a b =
    Point
        (a.x - b.x)
        (a.y - b.y)


{-| Returns the result of division of a given point by a given number.

    Point 50 30
        |> divideBy 5

    -- Point 10 6
-}
divideBy : Float -> Point -> Point
divideBy a b =
    Point
        (b.x / a)
        (b.y / a)


{-| Returns the result of multiplication of a given point by the given number.

    Point 50 30
        |> multiplyBy 2

    -- Point 100 60
-}
multiplyBy : Float -> Point -> Point
multiplyBy a b =
    Point
        (b.x * a)
        (b.y * a)


{-| Multiply each coordinate of point `a` by each coordinate of
point `b`. In linear algebra terms, multiply the point by the
[scaling matrix](https://en.wikipedia.org/wiki/Scaling_%28geometry%29#Matrix_representation)
defined by point `b`.

    Point 10 15
        |> scaleBy (Point 10 10)

    -- Point 100 150
-}
scaleBy : Point -> Point -> Point
scaleBy a b =
    Point
        (a.x * b.x)
        (a.y * b.y)


{-| Inverse of `scaleBy`. Divide each coordinate of point `b` by
each coordinate of point `a`.

    Point 100 150
        |> unscaleBy (Point 10 10)

    -- Point 10 15
-}
unscaleBy : Point -> Point -> Point
unscaleBy a b =
    Point
        (b.x / a.x)
        (b.y / a.y)


{-| Returns a point with rounded coordinates.

    round (Point 1.3 2.7) == Point 1 3
-}
round : Point -> Point
round a =
    Point
        (toFloat (Basics.round a.x))
        (toFloat (Basics.round a.y))


{-| Returns a point with floored coordinates (rounded down).

    floor (Point 1.3 2.7) == Point 1 2
-}
floor : Point -> Point
floor a =
    Point
        (toFloat (Basics.floor a.x))
        (toFloat (Basics.floor a.y))


{-| Returns a point with ceiled coordinates (rounded up).

    ceil (Point 1.3 2.7) == Point 2 3
-}
ceil : Point -> Point
ceil a =
    Point
        (toFloat (Basics.ceiling a.x))
        (toFloat (Basics.ceiling a.y))


{-| Returns the cartesian distance between two points.

    Point 0 30
      |> distanceTo (Point 40 0) -- 50
-}
distanceTo : Point -> Point -> Float
distanceTo a b =
    let
        x =
            b.x - a.x

        y =
            b.y - a.y
    in
        sqrt (x * x + y * y)


{-| Returns `True` if the given points has the same coordinates
and `False` otherwise.

    Point 20.4 50.12
      |> equals (Point 20.4 50.12) -- True

    Point 20.4 50.12
      |> equals (Point 20.5 50.13) -- False
-}
equals : Point -> Point -> Bool
equals a b =
    (a.x == b.x) && (a.y == b.y)


{-| Returns `True` if both coordinates of `a` are less
than the corresponding `b` coordinates (in absolute values).

    Point 50 30
      |> contains (Point -40 20) -- True

    Point 50 30
      |> contains (Point 60 -20) -- False

    Point 50 30
      |> contains (Point -40 -40) -- False
-}
contains : Point -> Point -> Bool
contains a b =
    ((abs a.x) <= (abs b.x)) && ((abs a.y) <= (abs b.y))


{-| Returns a `String` representation of the point for debugging purposes.

    Point 50.12345 30.98765
      |> toString 3         -- "Point(50.123, 30.987)"

    Point 50.12345 30.98765
      |> toString 0         -- "Point(50, 30)"
-}
toString : Int -> Point -> String
toString decimals point =
    let
        latString : String
        latString =
            formatFloat (locale decimals) point.x

        lngString : String
        lngString =
            formatFloat (locale decimals) point.y
    in
        "Point(" ++ latString ++ ", " ++ lngString ++ ")"


locale : Int -> Locale
locale decimalPlaces =
    { decimals = decimalPlaces
    , thousandSeparator = ""
    , decimalSeparator =
        if decimalPlaces > 0 then
            "."
        else
            ""
    }
