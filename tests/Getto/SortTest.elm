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
          ( Just "id", Just "down" ) |> Sort.fromString |> Maybe.map Sort.expose
          |> Expect.equal
            ( Just ( "id", "down") )

      , test "should decode from ( column, 'up' )" <|
        \_ ->
          ( Just "id", Just "up" ) |> Sort.fromString |> Maybe.map Sort.expose
          |> Expect.equal
            ( Just ( "id", "up" ) )

      , test "should return Nothing from ( column, 'unknown-order' )" <|
        \_ ->
          ( Just "id", Just "unknown-order" ) |> Sort.fromString |> Maybe.map Sort.expose
          |> Expect.equal
            Nothing

      , test "should return Nothing from ( column, Nothing )" <|
        \_ ->
          ( Just "id", Nothing ) |> Sort.fromString
          |> Expect.equal
            Nothing

      , test "should return Nothing from ( Nothing, 'down' )" <|
        \_ ->
          ( Nothing, Just "down" ) |> Sort.fromString
          |> Expect.equal
            Nothing
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
