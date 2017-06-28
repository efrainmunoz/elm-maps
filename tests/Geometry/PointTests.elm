module Geometry.PointTests exposing (geometryPoint)

import Test exposing (..)
import Expect
import Geometry.Point as Point exposing (..)


geometryPoint : Test
geometryPoint =
    describe "Geometry.Point"
        [ describe "add"
            [ test "adds given point to this one" <|
                \() ->
                    let
                        a : Point
                        a =
                            Point 50 30

                        b : Point
                        b =
                            Point 20 10
                    in
                        Expect.equal (Point.add a b) (Point 70 40)
            ]
        , describe "subtract"
            [ test "subtracts the given point from this one" <|
                \() ->
                    let
                        a : Point
                        a =
                            Point 50 30

                        b : Point
                        b =
                            Point 20 10
                    in
                        Expect.equal (Point.subtract a b) (Point 30 20)
            ]
        , describe "divideBy"
            [ test "divides a point by the given amount" <|
                \() ->
                    Point 50 30
                        |> Point.divideBy 5
                        |> Expect.equal (Point 10 6)
            ]
        , describe "multiplyBy"
            [ test "multiplies this point by the given amount" <|
                \() ->
                    Point 50 30
                        |> Point.multiplyBy 2
                        |> Expect.equal (Point 100 60)
            ]
        , describe "scaleBy"
            [ test "multiply the point by the [scaling matrix]" <|
                \() ->
                    Point 10 15
                        |> Point.scaleBy (Point 10 10)
                        |> Expect.equal (Point 100 150)
            ]
        , describe "unscaleBy"
            [ test "the inverse of scaleBy." <|
                \() ->
                    Point 100 150
                        |> Point.unscaleBy (Point 10 10)
                        |> Expect.equal (Point 10 15)
            ]
        , describe "floor"
            [ test "returns a new point with floored coordinates" <|
                \() ->
                    Expect.equal
                        (Point.floor <| Point 1.3 2.7)
                        (Point 1 2)
            ]
        , describe "round"
            [ test "returns a new point with rounded coordinates" <|
                \() ->
                    Expect.equal
                        (Point.round <| Point 1.3 2.7)
                        (Point 1 3)
            ]
        , describe "ceil"
            [ test "returns a new point with ceiled coordinates" <|
                \() ->
                    Expect.equal
                        (Point.ceil <| Point 1.3 2.7)
                        (Point 2 3)
            ]
        , describe "distanceTo"
            [ test "returns a new point with ceiled coordinates" <|
                \() ->
                    Point 0 30
                        |> Point.distanceTo (Point 40 0)
                        |> Expect.equal 50
            ]
        , describe "equals"
            [ test "returns True if the given points has the same coordinates" <|
                \() ->
                    Point 20.4 50.12
                        |> Point.equals (Point 20.4 50.12)
                        |> Expect.equal True
            , test "returns False if the given points does not has the same coordinates" <|
                \() ->
                    Point 20.4 50.12
                        |> Point.equals (Point 20.5 50.13)
                        |> Expect.equal False
            ]
        , describe "contains"
            [ test "returns True if both coordinates of point a are less than the corresponding point b coordinates (in absolute values)" <|
                \() ->
                    Point 50 30
                        |> Point.contains (Point -40 20)
                        |> Expect.equal True
            , test "returns False if both coordinates of point a are not less than the corresponding point b coordinates (in absolute values)" <|
                \() ->
                    Point 50 30
                        |> Point.contains (Point -40 -40)
                        |> Expect.equal False
            ]
        , describe "toString"
            [ test "returns a String representation of the point" <|
                \() ->
                    Expect.equal "Point(50, 30)"
                        (Point.toString 0 (Point 50 30))
            , test "returns a String representation of the point" <|
                \() ->
                    Expect.equal "Point(50.000, 30.000)"
                        (Point.toString 3 (Point 50 30))
            ]
        ]
