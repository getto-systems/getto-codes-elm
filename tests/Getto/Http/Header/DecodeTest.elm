module Getto.Http.Header.DecodeTest exposing (..)
import Getto.Http.Header.Decode as HeaderDecode

import Dict

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

type alias Header =
  { contentType : String
  , modelId     : Int
  , etag        : Maybe String
  , name        : Maybe String
  , age         : Maybe Int
  }

suite : Test
suite =
  describe "Decode"
    [ describe "decode"
      [ test "should decode headers" <|
        \_ ->
          let
            headers =
              [ ( "Content-Type", "text/html" )
              , ( "X-Model-ID",   "3" )
              , ( "ETag",         "ETAG-VALUE" )
              ] |> Dict.fromList
            decoder =
              HeaderDecode.map5 Header
                ( HeaderDecode.at "Content-Type" HeaderDecode.string )
                ( HeaderDecode.at "X-Model-ID"   HeaderDecode.int )
                ( HeaderDecode.at "ETag"        (HeaderDecode.maybe HeaderDecode.string) )
                ( HeaderDecode.at "X-Name"      (HeaderDecode.maybe HeaderDecode.string) )
                ( HeaderDecode.at "X-Age"       (HeaderDecode.maybe HeaderDecode.int) )
          in
            headers |> HeaderDecode.decode decoder
            |> Expect.equal
              ( Ok
                { contentType = "text/html"
                , modelId     = 3
                , etag        = Just "ETAG-VALUE"
                , name        = Nothing
                , age         = Nothing
                }
              )
      ]
    ]
