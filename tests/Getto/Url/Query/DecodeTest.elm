module Getto.Url.Query.DecodeTest exposing (..)
import Getto.Url.Query.Decode as Decode

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Decode"
    [ describe "decode"
      [ test "should decode query" <|
        \_ ->
          "q[name]=John&q[age]=30&q[en]&q[roles][]=admin&q[roles][]=system&q[nums][]=1&q[nums][]=value&s=name.desc"
          |> Decode.split
          |>
            (\query ->
              { query =
                { name     = query |> Decode.entryAt ["q","name"] (Decode.string "")
                , none     = query |> Decode.entryAt ["q","none"] (Decode.string "")
                , age      = query |> Decode.entryAt ["q","age"]  (Decode.int 0)
                , zero     = query |> Decode.entryAt ["q","zero"] (Decode.int 0)
                , enabled  = query |> Decode.boolAt  ["q","en"]
                , disabled = query |> Decode.boolAt  ["q","dis"]
                , roles    = query |> Decode.listAt  ["q","roles"] (Decode.string "")
                , numbers  = query |> Decode.listAt  ["q","nums"]  (Decode.int 0)
                , empty    = query |> Decode.listAt  ["q","empty"] (Decode.string "")
                }
              , sort = query |> Decode.entryAt ["s"] (Decode.string "")
              }
            )
          |> Expect.equal
            { query =
              { name     = "John"
              , none     = ""
              , age      = 30
              , zero     = 0
              , enabled  = True
              , disabled = False
              , roles    = ["admin","system"]
              , numbers  = [1,0]
              , empty    = []
              }
            , sort = "name.desc"
            }

      , test "should decode simple string" <|
        \_ ->
          Just "value"
          |> Decode.string ""
          |> Expect.equal "value"

      , test "should return default string with nothing" <|
        \_ ->
          Nothing
          |> Decode.string ""
          |> Expect.equal ""

      , test "should decode simple int" <|
        \_ ->
          Just "12"
          |> Decode.int 0
          |> Expect.equal 12

      , test "should return default int with nothing" <|
        \_ ->
          Nothing
          |> Decode.int 0
          |> Expect.equal 0

      , test "should return empty dict if decode failed" <|
        \_ ->
          Just ""
          |> Decode.int 0
          |> Expect.equal 0

      , test "should decode boolean true" <|
        \_ ->
          "value"
          |> Decode.split |> (Decode.boolAt ["value"])
          |> Expect.equal True

      , test "should decode boolean false" <|
        \_ ->
          ""
          |> Decode.split |> (Decode.boolAt ["value"])
          |> Expect.equal False

      , test "should decode special chars" <|
        \_ ->
          "%3F%5B%20%5D%3D%26=%5B%20%5D%3D%26%3F"
          |> Decode.split |> (Decode.entryAt ["?[ ]=&"] (Decode.string ""))
          |> Expect.equal "[ ]=&?"

      , test "should decode first entry with several entries" <|
        \_ ->
          "entry=value&entry=value2&entry=value3"
          |> Decode.split |> (Decode.entryAt ["entry"] (Decode.string ""))
          |> Expect.equal "value"

      , test "should decode first entry with several entries that decode successful or failed" <|
        \_ ->
          "number=value&number=2&number=3"
          |> Decode.split |> (Decode.entryAt ["number"] (Decode.int 0))
          |> Expect.equal 0
      ]
    ]
