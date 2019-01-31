module GettoUpload.Layout.Frame.Side.ViewTest exposing (..)
import GettoUpload.Layout.Frame.Side.View as Side
import GettoUpload.Layout.Menu as Menu
import GettoUpload.Layout.Fa as Fa

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

menu : Menu.Menu
menu =
  [ ( "main"
    , [ Menu.item (Fa.solid "home") "home.html"
        [ Menu.item (Fa.solid "file") "home/file.html"
          [ Menu.item (Fa.solid "pencil") "home/file/edit.html" []
          ]
        ]
      , Menu.item (Fa.solid "data") "data.html"
        [ Menu.item (Fa.solid "file") "data/file.html"
          [ Menu.item (Fa.solid "pencil") "data/file/edit.html" []
          ]
        ]
      ]
    )
  , ( "data"
    , [ Menu.item (Fa.solid "home") "master.html"
        [ Menu.item (Fa.solid "file") "master/file.html"
          [ Menu.item (Fa.solid "pencil") "master/file/edit.html" []
          ]
        ]
      , Menu.item (Fa.solid "data") "upload.html"
        [ Menu.item (Fa.solid "file") "upload/file.html"
          [ Menu.item (Fa.solid "pencil") "upload/file/edit.html" []
          ]
        ]
      ]
    )
  ]

i18n =
  { menu = identity
  , title = identity
  }

suite : Test
suite =
  describe "Side"
    {-
    [ describe "side"
      [ test "should return side menu" <|
        \_ ->
          { path = "home.html" } |> Side.menu i18n menu
          |> Expect.equal
            (Just
              ( "main"
              , [ { title = "home.html", icon = Fa.solid "home", href = "home.html" }
                ]
              )
            )
      ]
    -}

    [ describe "breadcrumb"
      [ test "should return breadcrumbs" <|
        \_ ->
          { path = "home.html", menu = menu, i18n = i18n } |> Side.breadcrumb
          |> Expect.equal
            (Just
              ( "main"
              , [ { title = "home.html", icon = Fa.solid "home", href = "home.html" }
                ]
              )
            )

      , test "should return breadcrumbs at second level" <|
        \_ ->
          { path = "home/file.html", menu = menu, i18n = i18n } |> Side.breadcrumb
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
          { path = "home/file/edit.html", menu = menu, i18n = i18n } |> Side.breadcrumb
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
          { path = "master/file/edit.html", menu = menu, i18n = i18n } |> Side.breadcrumb
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
          { path = "upload.html", menu = menu, i18n = i18n } |> Side.breadcrumb
          |> Expect.equal
            (Just
              ( "data"
              , [ { title = "upload.html", icon = Fa.solid "data", href = "upload.html" }
                ]
              )
            )
      ]
    ]
