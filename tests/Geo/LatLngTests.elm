module Geo.LatLngTests exposing (geoLatLng)

import Test exposing (..)
import Expect
import Geo.LatLng as LatLng exposing (..)


geoLatLng : Test
geoLatLng =
    describe "Geo.LatLng"
        [ describe "equals"
            [ test "returns true if compared objects are equal within a certain margin" <|
                \() ->
                    let
                        a : LatLng
                        a =
                            LatLng 10 20 Nothing

                        b : LatLng
                        b =
                            LatLng (10 + 10 ^ -10) (20 + 10 ^ -10) Nothing
                    in
                        Expect.equal (LatLng.equals a b Nothing) True
            , test "returns false if compared objects are not equal within a certain margin" <|
                \() ->
                    let
                        a : LatLng
                        a =
                            LatLng 10 20 Nothing

                        b : LatLng
                        b =
                            LatLng 10 23.3 Nothing
                    in
                        Expect.equal (LatLng.equals a b Nothing) False
            ]
        , describe "toString"
            [ test "formats a string" <|
                \() ->
                    let
                        a : LatLng
                        a =
                            LatLng 10.333333333 20.2222222 Nothing
                    in
                        Expect.equal (LatLng.toString 3 a) "LatLng(10.333, 20.222)"
            ]
        ]
