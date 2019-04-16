module GettoCodes.View.Http.ProgressTest exposing (..)
import GettoCodes.View.Http.Progress as Progress

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Progress"
    [ describe "percentage"
      [ test "should return percentage of progress" <|
        \_ ->
          [ { current = 0, size = 100 } |> Progress.percentage
          , { current = 10, size = 100 } |> Progress.percentage
          , { current = 20, size = 100 } |> Progress.percentage
          , { current = 40, size = 100 } |> Progress.percentage
          , { current = 80, size = 100 } |> Progress.percentage
          , { current = 100, size = 100 } |> Progress.percentage
          ]
          |> Expect.equal
            [ 0
            , 10
            , 20
            , 40
            , 80
            , 100
            ]
      ]
    ]
