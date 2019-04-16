module GettoCodes.View.ValidateTest exposing (..)
import GettoCodes.View.Validate as Validate

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Validate"
    [ describe "conflict"
      [ test "should return conflict error" <|
        \_ ->
          Validate.conflict
          |> Expect.equal "conflict"
      ]
    ]
