module GettoUpload.Layout.Page.Side.Html exposing
  ( mobileHeader
  , breadcrumb
  , navHeader
  , navFooter
  , navAddress
  , nav
  )
import GettoUpload.Layout.Page.Side.View as View
import GettoUpload.Layout.View.Icon as Icon
import GettoUpload.Layout.View.Html as Html
import GettoUpload.Layout.Href as Href

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

mobileHeader : View.MobileHeader -> Html msg
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

navHeader : View.NavHeader -> Html msg
navHeader model =
  H.header []
    [ H.p []
      [ H.small [] [ model.company |> H.text ]
      , H.br [] []
      , model.title |> H.text
      , H.br [] []
      , H.small [] [ H.small [] [ model.sub |> H.text ] ]
      ]
    ]

navFooter : View.NavFooter -> Html msg
navFooter model =
  H.footer [] [ H.p [] [ "version : " ++ model.version |> H.text ] ]

navAddress : View.NavAddress -> Html msg
navAddress model =
  let
    mode m =
      let
        state =
          if m.state
            then "success"
            else "gray"
      in
          H.li []
            [ H.span [ state |> A.class ]
              [ Icon.fas "circle" |> Html.icon []
              , " " |> H.text
              , m.title |> model.i18n.mode |> H.text
              ]
            ]

    badge =
      case model.badge of
        View.NoProbrem -> "" |> H.text
        View.Loading   -> Icon.fas "spinner" |> Html.icon ["fa-pulse"]
        View.Failure error ->
          error |> Html.badge ["is-small","is-danger"]
  in
    H.address []
      [ H.a [ model.href.config |> Href.toString |> A.href ]
        [ H.ul []
          [ H.li [ "header" |> A.class ] [ H.span [] [ model.title |> model.i18n.title |> H.text ] ]
          , model.mode1 |> mode
          , model.mode2 |> mode
          ]
        ]
      , H.footer []
        [ badge
        , " " |> H.text
        , H.a [ model.href.profile |> Href.toString |> A.href ]
          [ Icon.fas "user-circle" |> Html.icon []
          , " " |> H.text
          , model.roles |> List.map model.i18n.role |> String.join " " |> H.text
          ]
        ]
      ]

breadcrumb : Maybe View.Breadcrumb -> Html msg
breadcrumb data =
  case data of
    Nothing -> "" |> H.text
    Just (title,items) ->
      H.ul [] <|
        [ H.li [] [ title |> H.text ] ] ++
        ( items |> List.map
          (\item ->
            H.li []
              [ H.a [ item.href |> Href.toString |> A.href ]
                [ item.icon |> Html.icon []
                , " " |> H.text
                , item.title |> H.text
                ]
              ]
          )
        )

nav : View.Nav msg -> Html msg
nav model =
  let
    onClick item =
      if item.collapsed
        then item.name |> model.open  |> E.onClick
        else item.name |> model.close |> E.onClick

    badge count =
      case count of
        Nothing    -> "" |> H.text
        Just value -> value |> String.fromInt |> Html.badge ["is-danger","is-small"]

    caret item =
      if item.collapsed
        then Icon.fas "caret-left" |> Html.icon []
        else Icon.fas "caret-down" |> Html.icon []

    active item =
      if item.active
        then "is-active"
        else ""
  in
    model.menu
    |> List.map
      (\header ->
        H.ul [] <|
          [ H.li []
            [ H.b []
              [ H.a [ "#" |> A.href, header |> onClick ]
                [ header.title |> H.text
                , " " |> H.text
                , header.badge |> badge
                , " " |> H.text
                , header |> caret
                ]
              ]
            ]
          ] ++
          ( header.items |> List.map
            (\item ->
              H.li [ item |> active |> A.class ]
                [ H.a [ item.href |> Href.toString |> A.href ]
                  [ item.icon |> Html.icon ["fa-fw"]
                  , " " |> H.text
                  , item.title |> H.text
                  , " " |> H.text
                  , item.badge |> badge
                  ]
                ]
            )
          )
      )
    |> H.section []
