module GettoCodes.View.IconTest exposing (..)
import GettoCodes.View.Icon as IconView

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Icon"
    [ describe "fas"
      [ test "should return fa-solid icon" <|
        \_ ->
          "user" |> IconView.fas
          |> Expect.equal
            (IconView.Fa (IconView.FaSolid "user"))
      ]

    , describe "far"
      [ test "should return fa-regular icon" <|
        \_ ->
          "user" |> IconView.far
          |> Expect.equal
            (IconView.Fa (IconView.FaRegular "user"))
      ]

    , describe "spinner"
      [ test "should return spinner icon" <|
        \_ ->
          IconView.spinner
          |> Expect.equal
            (IconView.Fa (IconView.FaSolid "spinner"))
      ]

    , describe "edit"
      [ test "should return edit icon" <|
        \_ ->
          IconView.edit
          |> Expect.equal
            (IconView.Fa (IconView.FaRegular "edit"))
      ]
    ]
