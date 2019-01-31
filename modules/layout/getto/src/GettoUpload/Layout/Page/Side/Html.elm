module GettoUpload.Layout.Page.Side.Html exposing
  ( mobileHeader
  , nav
  )
import GettoUpload.Layout.Page.Side.View as View

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

mobileHeader : { company : String, title : String, sub : String } -> Html msg
mobileHeader model =
  H.header []
    [ H.p []
      [ H.small [] [ model.company |> H.text ]
      , H.wbr [] []
      , " " |> H.text
      , model.title |> H.text
      , H.wbr [] []
      , " " |> H.text
      , H.small [] [ H.small [] [ model.sub |> H.text ] ]
      ]
    ]

nav : (String -> msg) -> Html msg
nav msg =
  H.section []
    [ H.ul []
      [ H.li []
        [ H.b []
          [ H.a [ A.href "#", E.onClick (msg "MAIN") ]
            [ "MAIN" |> H.text
            , " " |> H.text
            , H.em [ A.class "badge is-danger is-small" ] [ "4" |> H.text ]
            , " " |> H.text
            , H.i [ A.class "fas fa-caret-down" ] []
            ]
          ]
        ]
      , H.li [ A.class "is-active" ]
        [ H.a [ A.href "index" ]
          [ H.i [ A.class "fas fa-fw fa-home" ] []
          , " " |> H.text
          , "Home" |> H.text
          , " " |> H.text
          , H.em [ A.class "badge is-danger is-small" ] [ "4" |> H.text ]
          ]
        ]
      ]
    , H.ul []
      [ H.li []
        [ H.b []
          [ H.a [ A.href "#", E.onClick (msg "DATA") ]
            [ "DATA" |> H.text
            , " " |> H.text
            , H.i [ A.class "fas fa-caret-down" ] []
            ]
          ]
        ]
      , H.li []
        [ H.a [ A.href "index" ]
          [ H.i [ A.class "fas fa-fw fa-home" ] []
          , " " |> H.text
          , "Home" |> H.text
          ]
        ]
      ]
    ]
