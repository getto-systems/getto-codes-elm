module GettoUpload.Layout.VersionTest exposing (..)
import GettoUpload.Layout.Version as Version

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Version"
    [ describe "data"
      [ test "should return version data" <|
        \_ ->
          Version.data
          |> .copyright
          |> Expect.equal "GETTO systems"
      ]
    ]
