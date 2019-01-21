module Getto.TransitTest exposing (..)
import Getto.Transit as Transit

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Transit"
    [ describe "none"
      [ test "should return none cmd" <|
        \_ ->
          let
            model = ()
          in
            model |> Transit.none
            |> Expect.equal ( (), Cmd.none )
      ]

    , describe "batch"
      [ test "should return batched cmd" <|
        \_ ->
          let
            model = ()
          in
            model |> Transit.batch (\m -> [])
            |> Expect.equal ( (), Cmd.none )
      ]
    ]
