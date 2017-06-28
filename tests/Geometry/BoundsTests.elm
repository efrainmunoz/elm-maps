module Geometry.BoundsTests exposing (geometryBounds)

import Test exposing (..)
import Expect
import Geometry.Point as Point exposing (..)
import Geometry.Bounds as Bounds exposing (..)


geometryBounds : Test
geometryBounds =
    let
        a =
            Bounds (Point 14 12) (Point 30 40)
    in
        describe "Geometry.Bounds"
            [ describe "extend"
                [ test "extends the bounds to contain the given point" <|
                    \() ->
                        let
                            expected =
                                Bounds (Point 14 12) (Point 50 40)
                        in
                            Point 50 20
                                |> Bounds.extend a
                                |> Expect.equal expected
                ]
            , describe "getCenter"
                [ test "returns the center point" <|
                    \() ->
                        Bounds.getCenter False a
                            |> Expect.equal (Point 22 26)
                ]
            , describe "getBottomLeft"
                [ test "returns the bottom-left point of the bounds." <|
                    \() ->
                        Bounds.getBottomLeft a
                            |> Expect.equal (Point 14 40)
                ]
            , describe "getTopRight"
                [ test "returns the top-right point of the bounds." <|
                    \() ->
                        Bounds.getTopRight a
                            |> Expect.equal (Point 30 12)
                ]
            , describe "getSize"
                [ test "returns the size of the bounds as point" <|
                    \() ->
                        Bounds.getSize a
                            |> Expect.equal (Point 16 28)
                ]
            ]
