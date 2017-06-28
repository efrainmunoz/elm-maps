module Tests exposing (all)

import Test exposing (Test, describe)
import Geo.LatLngTests exposing (geoLatLng)
import Geometry.BoundsTests exposing (geometryBounds)
import Geometry.PointTests exposing (geometryPoint)


all : Test
all =
    describe "leaflet"
        [ geoLatLng
        , geometryBounds
        , geometryPoint
        ]
