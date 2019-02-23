module Getto.SortTest exposing (..)
import Getto.Sort as Sort

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Sort"
    [ describe "by"
      [ test "should return Sort.Model of initial column, down order" <|
        \_ ->
          "id" |> Sort.by |> Sort.expose
          |> Expect.equal
            ( "id"
            , "down"
            )
      ]
    , describe "fromString"
      [ test "should decode from ( column, 'down' )" <|
        \_ ->
          ( "id", "down" ) |> Sort.fromString |> Sort.expose
          |> Expect.equal
            ( "id"
            , "down"
            )

      , test "should decode from ( column, 'up' )" <|
        \_ ->
          ( "id", "up" ) |> Sort.fromString |> Sort.expose
          |> Expect.equal
            ( "id"
            , "up"
            )

      , test "should decode from ( column, 'unknown-order' )" <|
        \_ ->
          ( "id", "unknown-order" ) |> Sort.fromString |> Sort.expose
          |> Expect.equal
            ( "id"
            , "down"
            )
      ]
    , describe "state"
      [ test "should return current order and next sort" <|
        \_ ->
          let
            state = "id" |> Sort.state ("id" |> Sort.by)
          in
            ( state |> Sort.current
            , state |> Sort.next |> Sort.expose
            )
            |> Expect.equal
              ( Just Sort.Down
              , ( "id"
                , "up"
                )
              )
      , test "should return Nothing and default sort with different target column" <|
        \_ ->
          let
            state = "other" |> Sort.state ("id" |> Sort.by)
          in
            ( state |> Sort.current
            , state |> Sort.next |> Sort.expose
            )
            |> Expect.equal
              ( Nothing
              , ( "other"
                , "down"
                )
              )
      ]
    ]
