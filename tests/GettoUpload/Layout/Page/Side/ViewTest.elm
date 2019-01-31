module GettoUpload.Layout.Page.Side.ViewTest exposing (..)
import GettoUpload.Layout.Page.Side.View as Side
import GettoUpload.Layout.Menu as Menu
import GettoUpload.Layout.Icon as Icon

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

menu : Menu.Menu
menu =
  [ ( "main"
    , [ Menu.item (Icon.fas "home") "home.html"
        [ Menu.item (Icon.fas "file") "home/file.html"
          [ Menu.item (Icon.fas "pencil") "home/file/edit.html" []
          ]
        ]
      , Menu.item (Icon.fas "data") "data.html"
        [ Menu.item (Icon.fas "file") "data/file.html"
          [ Menu.item (Icon.fas "pencil") "data/file/edit.html" []
          ]
        ]
      ]
    )
  , ( "data"
    , [ Menu.item (Icon.fas "home") "master.html"
        [ Menu.item (Icon.fas "file") "master/file.html"
          [ Menu.item (Icon.fas "pencil") "master/file/edit.html" []
          ]
        ]
      , Menu.item (Icon.fas "data") "upload.html"
        [ Menu.item (Icon.fas "file") "upload/file.html"
          [ Menu.item (Icon.fas "pencil") "upload/file/edit.html" []
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
              , [ { title = "home.html", icon = Icon.fas "home", href = "home.html" }
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
              , [ { title = "home.html", icon = Icon.fas "home", href = "home.html" }
                ]
              )
            )

      , test "should return breadcrumbs at second level" <|
        \_ ->
          { path = "home/file.html", menu = menu, i18n = i18n } |> Side.breadcrumb
          |> Expect.equal
            (Just
              ( "main"
              , [ { title = "home.html", icon = Icon.fas "home", href = "home.html" }
                , { title = "home/file.html", icon = Icon.fas "file", href = "home/file.html" }
                ]
              )
            )

      , test "should return breadcrumbs at therd level" <|
        \_ ->
          { path = "home/file/edit.html", menu = menu, i18n = i18n } |> Side.breadcrumb
          |> Expect.equal
            (Just
              ( "main"
              , [ { title = "home.html", icon = Icon.fas "home", href = "home.html" }
                , { title = "home/file.html", icon = Icon.fas "file", href = "home/file.html" }
                , { title = "home/file/edit.html", icon = Icon.fas "pencil", href = "home/file/edit.html" }
                ]
              )
            )

      , test "should return breadcrumbs at therd level of 'data'" <|
        \_ ->
          { path = "master/file/edit.html", menu = menu, i18n = i18n } |> Side.breadcrumb
          |> Expect.equal
            (Just
              ( "data"
              , [ { title = "master.html", icon = Icon.fas "home", href = "master.html" }
                , { title = "master/file.html", icon = Icon.fas "file", href = "master/file.html" }
                , { title = "master/file/edit.html", icon = Icon.fas "pencil", href = "master/file/edit.html" }
                ]
              )
            )

      , test "should return breadcrumbs at top level of another 'data'" <|
        \_ ->
          { path = "upload.html", menu = menu, i18n = i18n } |> Side.breadcrumb
          |> Expect.equal
            (Just
              ( "data"
              , [ { title = "upload.html", icon = Icon.fas "data", href = "upload.html" }
                ]
              )
            )
      ]
    ]
