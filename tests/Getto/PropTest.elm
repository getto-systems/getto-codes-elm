module Getto.PropTest exposing (..)
import Getto.Prop as Prop exposing ( Prop )

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

x : Prop { a | x : Int } Int
x = Prop.create .x (\f m -> { m | x = m.x |> f })

suite : Test
suite =
  describe "Prop"
    [ describe "get"
      [ test "should get property" <|
        \_ ->
          { x = 1 }
          |> Prop.get x
          |> Expect.equal 1
      ]

    , describe "set"
      [ test "should set property" <|
        \_ ->
          { x = 1 }
          |> Prop.set x 2
          |> Expect.equal { x = 2 }
      ]

    , describe "update"
      [ test "should update property" <|
        \_ ->
          { x = 1 }
          |> Prop.update x (\value -> value + 10)
          |> Expect.equal { x = 11 }
      ]
    ]
