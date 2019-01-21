module GettoUpload.Layout.Storage.MenuTest exposing (..)
import GettoUpload.Layout.Storage.Menu as Menu

import Set

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

init : Menu.Model
init =
  { collapsed = Set.empty
  }

suite : Test
suite =
  describe "Menu"
    [ describe "encode and decode"
      [ test "should encode and successfly decode Model" <|
        \_ ->
          let
            model = init
              |> Menu.toggle "MAIN"
          in
            model |> Menu.encode |> Menu.decode
            |> Expect.equal model
      ]

    , describe "toggle and isCollapsed"
      [ test "should expanded when init" <|
        \_ ->
          init |> Menu.isCollapsed "MAIN"
          |> Expect.equal False

      , test "should collapsed when toggle" <|
        \_ ->
          init |> Menu.toggle "MAIN" |> Menu.isCollapsed "MAIN"
          |> Expect.equal True

      , test "should expanded when double toggle" <|
        \_ ->
          init |> Menu.toggle "MAIN" |> Menu.toggle "MAIN" |> Menu.isCollapsed "MAIN"
          |> Expect.equal False
      ]
    ]
