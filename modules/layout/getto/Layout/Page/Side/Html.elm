module GettoCodes.Layout.Page.Side.Html exposing
  ( mobileHeader
  , breadcrumb
  , navHeader
  , navFooter
  , navAddress
  , nav
  )
import GettoCodes.Layout.Page.Side.View as Side
import GettoCodes.Layout.Page.Options.View as Options
import GettoCodes.View.Http as HttpView
import GettoCodes.View.Html as Html
import GettoCodes.View.Icon as Icon
import GettoCodes.Extension.Href as Href exposing ( Href )

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


navHeader : { company : String, title : String, sub : String } -> Html msg
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


navFooter : { version : String } -> Html msg
navFooter model =
  H.footer [] [ H.p [] [ "version : " ++ model.version |> H.text ] ]


type alias NavAddress =
  { title : String
  , mode1 : NavAddressMode
  , mode2 : NavAddressMode
  , roles : List String
  , get :
    { side    : HttpView.Model Side.Response
    , options : HttpView.Model Options.Response
    }
  , href :
    { config  : Href
    , profile : Href
    }
  , i18n :
    { title : String -> String
    , mode  : String -> String
    , role  : String -> String
    , http  : HttpView.Error -> String
    }
  }
type alias NavAddressMode =
  { title : String
  , state : Bool
  }

navAddress : NavAddress -> Html msg
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

    badge get =
      case get |> HttpView.state of
        HttpView.Ready (Just error) -> error |> model.i18n.http |> Html.badge ["is-small","is-danger"]
        HttpView.Connecting _       -> Html.spinner
        _ -> "" |> H.text
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
        [ model.get.side |> badge
        , " " |> H.text
        , model.get.options |> badge
        , " " |> H.text
        , H.a [ model.href.profile |> Href.toString |> A.href ]
          [ Icon.fas "user-circle" |> Html.icon []
          , " " |> H.text
          , model.roles |> List.map model.i18n.role |> String.join " " |> H.text
          ]
        ]
      ]


breadcrumb : Maybe Side.Breadcrumb -> Html msg
breadcrumb data =
  case data of
    Nothing -> "" |> H.text
    Just (title,items) ->
      H.ul []
        ( [ [ H.li [] [ title |> H.text ] ]
          , items |> List.map
            (\item ->
              H.li []
                [ H.a [ item.href |> Href.toString |> A.href ]
                  [ item.icon |> Html.icon []
                  , " " |> H.text
                  , item.title |> H.text
                  ]
                ]
            )
          ]
          |> List.concat
          |> List.intersperse (H.li [] [ Icon.fas "caret-right" |> Html.icon ["fa-fw"] ])
        )


type alias NavModel msg =
  { menu : List Side.Menu
  , msg :
    { open : String -> msg
    , close : String -> msg
    }
  }

nav : NavModel msg -> Html msg
nav model =
  let
    class item =
      if item.collapsed
        then [ "is-collapsed" |> A.class ]
        else []

    onClick item =
      if item.collapsed
        then item.name |> model.msg.open  |> E.onClick
        else item.name |> model.msg.close |> E.onClick

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
        H.ul ( header |> class ) <|
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
