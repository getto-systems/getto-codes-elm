module Getto.Json.DecodeTest exposing (..)
import Getto.Json.Decode as Decode

import Json.Encode as Encode

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Decode"
    [ describe "at"
      [ test "should decode query" <|
        \_ ->
          let
            value =
              [ ( "q"
                , [ ( "name", "John" |> Encode.string )
                  , ( "age",  30     |> Encode.int )
                  , ( "en",   True   |> Encode.bool )
                  , ( "roles", [ "admin", "system" ] |> Encode.list Encode.string )
                  , ( "nums",  [ 1,2,3 ] |> Encode.list Encode.int )
                  ] |> Encode.object
                )
              , ( "s", "name.desc" |> Encode.string )
              ] |> Encode.object
          in
            { query =
              { name     = value |> Decode.at ["q","name"]  (Decode.string "")
              , none     = value |> Decode.at ["q","none"]  (Decode.string "")
              , age      = value |> Decode.at ["q","age"]   (Decode.int 0)
              , zero     = value |> Decode.at ["q","zero"]  (Decode.int 0)
              , enabled  = value |> Decode.at ["q","en"]    (Decode.bool False)
              , disabled = value |> Decode.at ["q","dis"]   (Decode.bool False)
              , roles    = value |> Decode.at ["q","roles"] (Decode.list (Decode.string ""))
              , numbers  = value |> Decode.at ["q","nums"]  (Decode.list (Decode.int 0))
              , empty    = value |> Decode.at ["q","empty"] (Decode.list (Decode.int 0))
              }
            , sort = value |> Decode.at ["s"] (Decode.string "")
            }
            |> Expect.equal
              { query =
                { name     = "John"
                , none     = ""
                , age      = 30
                , zero     = 0
                , enabled  = True
                , disabled = False
                , roles    = ["admin","system"]
                , numbers  = [1,2,3]
                , empty    = []
                }
              , sort = "name.desc"
              }
      ]

    , describe "valueAt"
      [ test "should get value at specified position" <|
        \_ ->
          [ ( "q"
            , [ ( "name", "John" |> Encode.string )
              , ( "age",  30     |> Encode.int )
              , ( "en",   True   |> Encode.bool )
              , ( "roles", [ "admin", "system" ] |> Encode.list Encode.string )
              , ( "nums",  [ 1,2,3 ] |> Encode.list Encode.int )
              ] |> Encode.object
            )
          , ( "s", "name.desc" |> Encode.string )
          ] |> Encode.object
          |> Decode.valueAt ["q"]
          |> Expect.equal
            ( [ ( "name", "John" |> Encode.string )
              , ( "age",  30     |> Encode.int )
              , ( "en",   True   |> Encode.bool )
              , ( "roles", [ "admin", "system" ] |> Encode.list Encode.string )
              , ( "nums",  [ 1,2,3 ] |> Encode.list Encode.int )
              ] |> Encode.object
            )
      ]
    ]
