module Getto.Json.SafeDecodeTest exposing (..)
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "SafeDecode"
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
              { name     = value |> SafeDecode.at ["q","name"]  (SafeDecode.string "")
              , none     = value |> SafeDecode.at ["q","none"]  (SafeDecode.string "")
              , age      = value |> SafeDecode.at ["q","age"]   (SafeDecode.int 0)
              , zero     = value |> SafeDecode.at ["q","zero"]  (SafeDecode.int 0)
              , enabled  = value |> SafeDecode.at ["q","en"]    (SafeDecode.bool False)
              , disabled = value |> SafeDecode.at ["q","dis"]   (SafeDecode.bool False)
              , roles    = value |> SafeDecode.at ["q","roles"] (SafeDecode.list (SafeDecode.string ""))
              , numbers  = value |> SafeDecode.at ["q","nums"]  (SafeDecode.list (SafeDecode.int 0))
              , empty    = value |> SafeDecode.at ["q","empty"] (SafeDecode.list (SafeDecode.int 0))
              }
            , sort = value |> SafeDecode.at ["s"] (SafeDecode.string "")
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
          |> SafeDecode.valueAt ["q"]
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
