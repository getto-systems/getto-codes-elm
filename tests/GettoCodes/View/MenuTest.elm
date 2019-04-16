module GettoCodes.View.MenuTest exposing (..)
import GettoCodes.View.Menu as Menu
import GettoCodes.View.Icon as IconView
import GettoCodes.View.Href as Href

import Getto.Url.Query.Encode as QueryEncode

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Menu"
    [ describe "item"
      [ test "should return menu item" <|
        \_ ->
          let
            sub = Menu.item IconView.edit ("path/to/sub.html" |> Href.internal QueryEncode.null) []
            item = Menu.item IconView.edit ("path/to/main.html" |> Href.internal QueryEncode.null) [ sub ]
          in
            ( item |> Menu.icon
            , item |> Menu.href
            , item |> Menu.children
            )
            |> Expect.equal
              ( IconView.edit
              , "path/to/main.html" |> Href.internal QueryEncode.null
              , [ sub ]
              )
      ]
    ]
