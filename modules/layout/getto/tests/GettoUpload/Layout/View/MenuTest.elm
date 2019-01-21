module GettoUpload.Layout.View.MenuTest exposing (..)
import GettoUpload.Layout.View.Menu as Menu
import GettoUpload.Layout.Menu.Model as Model
import GettoUpload.Layout.Fa as Fa

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

menu : Model.Menu
menu =
  [ ( "main"
    , [ Model.item (Fa.solid "home") "home.html"
        [ Model.item (Fa.solid "file") "home/file.html"
          [ Model.item (Fa.solid "pencil") "home/file/edit.html" []
          ]
        ]
      , Model.item (Fa.solid "data") "data.html"
        [ Model.item (Fa.solid "file") "data/file.html"
          [ Model.item (Fa.solid "pencil") "data/file/edit.html" []
          ]
        ]
      ]
    )
  , ( "data"
    , [ Model.item (Fa.solid "home") "master.html"
        [ Model.item (Fa.solid "file") "master/file.html"
          [ Model.item (Fa.solid "pencil") "master/file/edit.html" []
          ]
        ]
      , Model.item (Fa.solid "data") "upload.html"
        [ Model.item (Fa.solid "file") "upload/file.html"
          [ Model.item (Fa.solid "pencil") "upload/file/edit.html" []
          ]
        ]
      ]
    )
  ]

suite : Test
suite =
  describe "Menu"
    [ describe "breadcrumb"
      [ test "should return breadcrumbs" <|
        \_ ->
          menu |> Menu.breadcrumb { path = "home.html" }
          |> Expect.equal
            (Just
              ( "main"
              , [ { title = "home.html", icon = Fa.solid "home", href = "home.html" }
                ]
              )
            )

      , test "should return breadcrumbs at second level" <|
        \_ ->
          menu |> Menu.breadcrumb { path = "home/file.html" }
          |> Expect.equal
            (Just
              ( "main"
              , [ { title = "home.html", icon = Fa.solid "home", href = "home.html" }
                , { title = "home/file.html", icon = Fa.solid "file", href = "home/file.html" }
                ]
              )
            )

      , test "should return breadcrumbs at therd level" <|
        \_ ->
          menu |> Menu.breadcrumb { path = "home/file/edit.html" }
          |> Expect.equal
            (Just
              ( "main"
              , [ { title = "home.html", icon = Fa.solid "home", href = "home.html" }
                , { title = "home/file.html", icon = Fa.solid "file", href = "home/file.html" }
                , { title = "home/file/edit.html", icon = Fa.solid "pencil", href = "home/file/edit.html" }
                ]
              )
            )

      , test "should return breadcrumbs at therd level of 'data'" <|
        \_ ->
          menu |> Menu.breadcrumb { path = "master/file/edit.html" }
          |> Expect.equal
            (Just
              ( "data"
              , [ { title = "master.html", icon = Fa.solid "home", href = "master.html" }
                , { title = "master/file.html", icon = Fa.solid "file", href = "master/file.html" }
                , { title = "master/file/edit.html", icon = Fa.solid "pencil", href = "master/file/edit.html" }
                ]
              )
            )

      , test "should return breadcrumbs at top level of another 'data'" <|
        \_ ->
          menu |> Menu.breadcrumb { path = "upload.html" }
          |> Expect.equal
            (Just
              ( "data"
              , [ { title = "upload.html", icon = Fa.solid "data", href = "upload.html" }
                ]
              )
            )
      ]
    ]
