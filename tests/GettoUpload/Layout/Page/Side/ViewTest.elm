module GettoUpload.Layout.Page.Side.ViewTest exposing (..)
import GettoUpload.Layout.Page.Side.View as Side
import GettoUpload.Layout.View.Menu as Menu
import GettoUpload.Layout.View.Icon as Icon

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
    [ describe "menu"
      [ test "should return side menu" <|
        \_ ->
          let
            allow roles (group,_) = True
            collapsed group = False
            badge path = Nothing
          in
            { path = "home.html", roles = ["admin"], menu = menu, i18n = i18n
            , allow = allow, collapsed = collapsed, badge = badge } |> Side.menu
            |> Expect.equal
              [ { title     = "main"
                , badge     = Nothing
                , collapsed = False
                , items =
                  [ { active = True
                    , title  = "home.html"
                    , href   = "home.html"
                    , icon   = Icon.fas "home"
                    , badge  = Nothing
                    }
                  , { active = False
                    , title  = "data.html"
                    , href   = "data.html"
                    , icon   = Icon.fas "data"
                    , badge  = Nothing
                    }
                  ]
                }
              , { title     = "data"
                , badge     = Nothing
                , collapsed = False
                , items =
                  [ { active = False
                    , title  = "master.html"
                    , href   = "master.html"
                    , icon   = Icon.fas "home"
                    , badge  = Nothing
                    }
                  , { active = False
                    , title  = "upload.html"
                    , href   = "upload.html"
                    , icon   = Icon.fas "data"
                    , badge  = Nothing
                    }
                  ]
                }
              ]

      , test "should return side menu only allowed" <|
        \_ ->
          let
            allow roles (group,items) = group == "main"
            collapsed group = False
            badge path = Nothing
          in
            { path = "home.html", roles = ["admin"], menu = menu, i18n = i18n
            , allow = allow, collapsed = collapsed, badge = badge } |> Side.menu
            |> Expect.equal
              [ { title     = "main"
                , badge     = Nothing
                , collapsed = False
                , items =
                  [ { active = True
                    , title  = "home.html"
                    , href   = "home.html"
                    , icon   = Icon.fas "home"
                    , badge  = Nothing
                    }
                  , { active = False
                    , title  = "data.html"
                    , href   = "data.html"
                    , icon   = Icon.fas "data"
                    , badge  = Nothing
                    }
                  ]
                }
              ]

      , test "should return side menu only allowed with roles" <|
        \_ ->
          let
            allow roles (group,_) = roles |> List.member "admin"
            collapsed group = False
            badge path = Nothing
          in
            { path = "home.html", roles = ["admin"], menu = menu, i18n = i18n
            , allow = allow, collapsed = collapsed, badge = badge } |> Side.menu
            |> Expect.equal
              [ { title     = "main"
                , badge     = Nothing
                , collapsed = False
                , items =
                  [ { active = True
                    , title  = "home.html"
                    , href   = "home.html"
                    , icon   = Icon.fas "home"
                    , badge  = Nothing
                    }
                  , { active = False
                    , title  = "data.html"
                    , href   = "data.html"
                    , icon   = Icon.fas "data"
                    , badge  = Nothing
                    }
                  ]
                }
              , { title     = "data"
                , badge     = Nothing
                , collapsed = False
                , items =
                  [ { active = False
                    , title  = "master.html"
                    , href   = "master.html"
                    , icon   = Icon.fas "home"
                    , badge  = Nothing
                    }
                  , { active = False
                    , title  = "upload.html"
                    , href   = "upload.html"
                    , icon   = Icon.fas "data"
                    , badge  = Nothing
                    }
                  ]
                }
              ]

      , test "should return side menu with collapsed" <|
        \_ ->
          let
            allow roles (group,_) = True
            collapsed group = group == "data"
            badge path = Nothing
          in
            { path = "home.html", roles = ["admin"], menu = menu, i18n = i18n
            , allow = allow, collapsed = collapsed, badge = badge } |> Side.menu
            |> Expect.equal
              [ { title     = "main"
                , badge     = Nothing
                , collapsed = False
                , items =
                  [ { active = True
                    , title  = "home.html"
                    , href   = "home.html"
                    , icon   = Icon.fas "home"
                    , badge  = Nothing
                    }
                  , { active = False
                    , title  = "data.html"
                    , href   = "data.html"
                    , icon   = Icon.fas "data"
                    , badge  = Nothing
                    }
                  ]
                }
              , { title     = "data"
                , badge     = Nothing
                , collapsed = True
                , items = []
                }
              ]

      , test "should return side menu with collapsed on active tree" <|
        \_ ->
          let
            allow roles (group,_) = True
            collapsed group = group == "main"
            badge path = Nothing
          in
            { path = "home.html", roles = ["admin"], menu = menu, i18n = i18n
            , allow = allow, collapsed = collapsed, badge = badge } |> Side.menu
            |> Expect.equal
              [ { title     = "main"
                , badge     = Nothing
                , collapsed = True
                , items =
                  [ { active = True
                    , title  = "home.html"
                    , href   = "home.html"
                    , icon   = Icon.fas "home"
                    , badge  = Nothing
                    }
                  ]
                }
              , { title     = "data"
                , badge     = Nothing
                , collapsed = False
                , items =
                  [ { active = False
                    , title  = "master.html"
                    , href   = "master.html"
                    , icon   = Icon.fas "home"
                    , badge  = Nothing
                    }
                  , { active = False
                    , title  = "upload.html"
                    , href   = "upload.html"
                    , icon   = Icon.fas "data"
                    , badge  = Nothing
                    }
                  ]
                }
              ]

      , test "should return side menu with badge" <|
        \_ ->
          let
            allow roles (group,_) = True
            collapsed group = False
            badge path =
              case path of
                "home.html" -> Just 4
                _           -> Nothing
          in
            { path = "home.html", roles = ["admin"], menu = menu, i18n = i18n
            , allow = allow, collapsed = collapsed, badge = badge } |> Side.menu
            |> Expect.equal
              [ { title     = "main"
                , badge     = Just 4
                , collapsed = False
                , items =
                  [ { active = True
                    , title  = "home.html"
                    , href   = "home.html"
                    , icon   = Icon.fas "home"
                    , badge  = Just 4
                    }
                  , { active = False
                    , title  = "data.html"
                    , href   = "data.html"
                    , icon   = Icon.fas "data"
                    , badge  = Nothing
                    }
                  ]
                }
              , { title     = "data"
                , badge     = Nothing
                , collapsed = False
                , items =
                  [ { active = False
                    , title  = "master.html"
                    , href   = "master.html"
                    , icon   = Icon.fas "home"
                    , badge  = Nothing
                    }
                  , { active = False
                    , title  = "upload.html"
                    , href   = "upload.html"
                    , icon   = Icon.fas "data"
                    , badge  = Nothing
                    }
                  ]
                }
              ]

      , test "should return side menu with several badge" <|
        \_ ->
          let
            allow roles (group,_) = True
            collapsed group = False
            badge path =
              case path of
                "home.html" -> Just 4
                "data.html" -> Just 3
                _           -> Nothing
          in
            { path = "home.html", roles = ["admin"], menu = menu, i18n = i18n
            , allow = allow, collapsed = collapsed, badge = badge } |> Side.menu
            |> Expect.equal
              [ { title     = "main"
                , badge     = Just 7
                , collapsed = False
                , items =
                  [ { active = True
                    , title  = "home.html"
                    , href   = "home.html"
                    , icon   = Icon.fas "home"
                    , badge  = Just 4
                    }
                  , { active = False
                    , title  = "data.html"
                    , href   = "data.html"
                    , icon   = Icon.fas "data"
                    , badge  = Just 3
                    }
                  ]
                }
              , { title     = "data"
                , badge     = Nothing
                , collapsed = False
                , items =
                  [ { active = False
                    , title  = "master.html"
                    , href   = "master.html"
                    , icon   = Icon.fas "home"
                    , badge  = Nothing
                    }
                  , { active = False
                    , title  = "upload.html"
                    , href   = "upload.html"
                    , icon   = Icon.fas "data"
                    , badge  = Nothing
                    }
                  ]
                }
              ]

      , test "should return side menu with inner badge" <|
        \_ ->
          let
            allow roles (group,_) = True
            collapsed group = False
            badge path =
              case path of
                "home.html"      -> Just 4
                "home/file.html" -> Just 3
                _                -> Nothing
          in
            { path = "home.html", roles = ["admin"], menu = menu, i18n = i18n
            , allow = allow, collapsed = collapsed, badge = badge } |> Side.menu
            |> Expect.equal
              [ { title     = "main"
                , badge     = Just 7
                , collapsed = False
                , items =
                  [ { active = True
                    , title  = "home.html"
                    , href   = "home.html"
                    , icon   = Icon.fas "home"
                    , badge  = Just 7
                    }
                  , { active = False
                    , title  = "data.html"
                    , href   = "data.html"
                    , icon   = Icon.fas "data"
                    , badge  = Nothing
                    }
                  ]
                }
              , { title     = "data"
                , badge     = Nothing
                , collapsed = False
                , items =
                  [ { active = False
                    , title  = "master.html"
                    , href   = "master.html"
                    , icon   = Icon.fas "home"
                    , badge  = Nothing
                    }
                  , { active = False
                    , title  = "upload.html"
                    , href   = "upload.html"
                    , icon   = Icon.fas "data"
                    , badge  = Nothing
                    }
                  ]
                }
              ]
      ]

    , describe "breadcrumb"
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
