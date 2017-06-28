module Geo.LatLng
    exposing
        ( Lat
        , Lng
        , Alt
        , LatLng
        , MaxMargin
        , equals
        , toString
        )

{-| Represents a geographical point with a certain latitude and longitude.

# LatLng
@docs Lat, Lng, Alt, LatLng

# Comparison
@docs MaxMargin, equals

# Other Utilites
@docs toString

-}

import FormatNumber exposing (Locale, formatFloat)


{-| Latitude in degrees.
-}
type alias Lat =
    Float


{-| Longitude in degrees.
-}
type alias Lng =
    Float


{-| Altitude in meters.
-}
type alias Alt =
    Float


{-| Represents a geographical point with a certain latitude and longitude.
-}
type alias LatLng =
    { lat : Lat
    , lng : Lng
    , alt : Maybe Alt
    }


{-| Represents the maximum margin of error when using the `equals` function.
-}
type alias MaxMargin =
    Maybe Float


{-| Returns `true` if the given `LatLng` point is at the same position
(within a small margin of error). The margin of error can be overriden
by setting `MaxMargin` to a small number.
-}
equals : LatLng -> LatLng -> MaxMargin -> Bool
equals a b maxMargin =
    let
        margin : Float
        margin =
            max (abs <| a.lat - b.lat) (abs <| a.lng - b.lng)
    in
        maxMargin
            |> Maybe.withDefault (10 ^ -9)
            |> (<=) margin


{-| Returns a string representation of the point (for debugging purposes).
-}
toString : Int -> LatLng -> String
toString decimals latLng =
    let
        latString : String
        latString =
            formatFloat (locale decimals) latLng.lat

        lngString : String
        lngString =
            formatFloat (locale decimals) latLng.lng
    in
        "LatLng(" ++ latString ++ ", " ++ lngString ++ ")"


locale : Int -> Locale
locale decimalPlaces =
    { decimals = 3
    , thousandSeparator = ""
    , decimalSeparator = "."
    }
