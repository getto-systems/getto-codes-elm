module GettoCodes.View.HrefTest exposing (..)
import GettoCodes.View.Href as Href
import GettoCodes.Env as Env

import Getto.Url.Query.Encode as QueryEncode

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Href"
    [ describe "internal"
      [ test "should return internal href" <|
        \_ ->
          let
            href = "path/to/internal.html" |> Href.internal QueryEncode.null
          in
            ( href |> Href.path
            , href |> Href.toString
            )
            |> Expect.equal
              ( Href.Internal "path/to/internal.html"
              , Env.href.internal ++ "path/to/internal.html"
              )
      ]

    , describe "keycloak"
      [ test "should return keycloak href" <|
        \_ ->
          let
            href = "path/to/keycloak.html" |> Href.keycloak QueryEncode.null
          in
            ( href |> Href.path
            , href |> Href.toString
            )
            |> Expect.equal
              ( Href.Keycloak "path/to/keycloak.html"
              , Env.href.keycloak ++ "path/to/keycloak.html"
              )
      ]
    ]
