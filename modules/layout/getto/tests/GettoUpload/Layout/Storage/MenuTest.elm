module GettoUpload.Layout.Storage.MenuTest exposing (..)
import GettoUpload.Layout.Storage.Menu as Menu

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Json.Decode as Decode

suite : Test
suite =
  describe "Menu"
    [ describe "encode and decode"
      [ test "should encode and successfly decode Model" <|
        \_ ->
          let
            model = Menu.init
              |> Menu.toggle "MAIN"
          in
            model |> Menu.encode |> Decode.decodeValue Menu.decoder
            |> Expect.equal (Ok model)
      ]

    , describe "toggle and isCollapsed"
      [ test "should expanded when init" <|
        \_ ->
          Menu.init |> Menu.isCollapsed "MAIN"
          |> Expect.equal False

      , test "should collapsed when toggle" <|
        \_ ->
          Menu.init |> Menu.toggle "MAIN" |> Menu.isCollapsed "MAIN"
          |> Expect.equal True

      , test "should expanded when double toggle" <|
        \_ ->
          Menu.init |> Menu.toggle "MAIN" |> Menu.toggle "MAIN" |> Menu.isCollapsed "MAIN"
          |> Expect.equal False
      ]
    ]
