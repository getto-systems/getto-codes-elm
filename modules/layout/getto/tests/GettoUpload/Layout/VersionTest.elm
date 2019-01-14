module GettoUpload.Layout.VersionTest exposing (..)
import GettoUpload.Layout.Version as Version

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Version"
    [ describe "copyright"
      [ test "should equal to 'GETTO systems'" <|
        \_ ->
          Version.copyright
          |> Expect.equal "GETTO systems"
      ]
    ]
