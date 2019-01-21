module GettoUpload.App.Index.Page exposing ( main )
import GettoUpload.App.Index.Storage as Storage
import GettoUpload.App.Index.Query   as Query
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Href.Home as HomeHref
import GettoUpload.Layout.Version as Version
import GettoUpload.Env.App as Env
import GettoUpload.I18n.App as AppI18n

import Getto.Transit as Transit

import Browser
import Browser.Navigation as Navigation
import Url exposing ( Url )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

main = Browser.application
  { init          = Frame.init Storage.init Query.init init command
  , subscriptions = subscriptions
  , onUrlRequest  = UrlRequest
  , onUrlChange   = UrlChange
  , update        = update
  , view          = view
  }

type alias Model  = Frame.Model  Storage.Model Query.Model Page
type alias Plugin = Frame.Plugin Storage.Model Query.Model
type alias Page = {}

type Msg
  = UrlRequest Browser.UrlRequest
  | UrlChange Url
  | ToggleMenu String

init : ( Frame.Static, Frame.Layout, Plugin ) -> Page
init (static,layout,plugin) =
  {}

command : Model -> Model
command model = model

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
  case msg of
    UrlRequest urlRequest ->
      case urlRequest of
        Browser.Internal url ->  Transit.batch (\model -> [ url  |> Url.toString |> Navigation.load ])
        Browser.External href -> Transit.batch (\model -> [ href |> Navigation.load ])
    UrlChange url -> Transit.none

    ToggleMenu name -> Transit.none

view : Model -> Browser.Document Msg
view model =
  { title = model |> documentTitle
  , body = [ model |> L.lazy content ]
  }

documentTitle : Model -> String
documentTitle model =
  (model |> title)
  ++ " | "
  ++ model.static.project.company
  ++ " "
  ++ model.static.project.title

content : Model -> Html Msg
content model =
  H.div [ A.class "MainLayout" ] <|
    ( [ model |> mobileHeader
      , model |> navAddress
      , model |> article
      ] ++
      (model |> dialogs) ++
      [ H.nav []
        [ model |> navHeader
        , model |> navAddress
        , model |> nav
        , model |> navFooter
        ]
      ]
    )

title : Model -> String
title model =
  model.static.path |> AppI18n.title

mobileHeader : Model -> Html Msg
mobileHeader model =
  H.header []
    [ H.p []
      [ H.small [] [ model.static.project.company |> H.text ]
      , H.wbr [] []
      , " " |> H.text
      , model.static.project.title |> H.text
      , H.wbr [] []
      , " " |> H.text
      , H.small [] [ H.small [] [ model.static.project.sub |> H.text ] ]
      ]
    ]

navHeader : Model -> Html Msg
navHeader model =
  H.header []
    [ H.p []
      [ H.small [] [ model.static.project.company |> H.text ]
      , H.br [] []
      , model.static.project.title |> H.text
      , H.br [] []
      , H.small [] [ H.small [] [ model.static.project.sub |> H.text ] ]
      ]
    ]

navAddress : Model -> Html Msg
navAddress model =
  H.address []
    [ H.a [ A.href HomeHref.index ]
      [ H.ul []
        [ H.li [ A.class "header" ] [ H.span [] [ "Upload" |> H.text ] ]
        , H.li []
          [ H.span [ A.class "success" ]
            [ H.i [ A.class "fas fa-circle" ] []
            , " " |> H.text
            , "mode1" |> H.text
            ]
          ]
        , H.li []
          [ H.span [ A.class "gray" ]
            [ H.i [ A.class "fas fa-circle" ] []
            , " " |> H.text
            , "mode2" |> H.text
            ]
          ]
        ]
      ]
    , H.footer []
      [ H.a [ A.href HomeHref.index ]
        [ H.em [ A.class "badge is-small is-danger" ]
          [ "Error" |> H.text ] -- TODO ここに state の取得ステータスを表示
        ]
      , " " |> H.text
      , H.a [ A.href HomeHref.index ]
        [ H.i [ A.class "fas fa-user-circle" ] []
        , " " |> H.text
        , "manager" |> H.text
        ]
      ]
    ]

nav : Model -> Html Msg
nav model =
  H.section []
    [ H.ul []
      [ H.li []
        [ H.b []
          [ H.a [ A.href "#", E.onClick (ToggleMenu "MAIN") ]
            [ "MAIN" |> H.text
            , " " |> H.text
            , H.em [ A.class "badge is-danger is-small" ] [ "4" |> H.text ]
            , " " |> H.text
            , H.i [ A.class "fas fa-caret-down" ] []
            ]
          ]
        ]
      , H.li [ A.class "is-active" ]
        [ H.a [ A.href HomeHref.index ]
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
          [ H.a [ A.href "#", E.onClick (ToggleMenu "DATA") ]
            [ "DATA" |> H.text
            , " " |> H.text
            , H.i [ A.class "fas fa-caret-down" ] []
            ]
          ]
        ]
      , H.li []
        [ H.a [ A.href HomeHref.index ]
          [ H.i [ A.class "fas fa-fw fa-home" ] []
          , " " |> H.text
          , "Home" |> H.text
          ]
        ]
      ]
    ]

navFooter : Model -> Html Msg
navFooter model =
  H.footer [] [ H.p [] [ "version : " ++ Version.version |> H.text ] ]

article : Model -> Html Msg
article model =
  H.article [] <|
    [ H.header []
      [ H.h1 [] [ model |> title |> H.text ]
      , H.ul []
        [ H.li [] [ "MAIN" |> H.text ]
        , H.li []
          [ H.a [ A.href HomeHref.index ]
            [ H.i [ A.class "fas fa-home" ] []
            , " " |> H.text
            , "Home" |> H.text
            ]
          ]
        ]
      ]
    ] ++
    (model |> contents) ++
    [ H.footer []
      [ H.p []
        [ H.i [ A.class "far fa-copyright" ] []
        , " " |> H.text
        , Version.copyright |> H.text
        ]
      ]
    ]

contents : Model -> List (Html Msg)
contents model =
  [ model |> section
  ]

section : Model -> Html Msg
section model =
  H.section [ A.class "dashboard" ]
    [ H.dl []
      [ H.dd []
        [ H.section []
          [ H.h2 [] [ "Subject" |> H.text ]
          , H.p []
            [ H.em []
              [ "3" |> H.text
              , H.small [] [ H.small [] [ "million" |> H.text ] ]
              ]
            , " " |> H.text
            , H.em [] [ "/" |> H.text ]
            , " " |> H.text
            , H.em []
              [ H.small [] [ "20" |> H.text ]
              , H.small [] [ H.small [] [ "million" |> H.text ] ]
              ]
            ]
          , H.meter [ A.value "3", A.max "20" ] [ "15%" |> H.text ]
          , H.footer []
            [ H.a [ A.href HomeHref.index ]
              [ H.i [ A.class "fas fa-user" ] []
              , " " |> H.text
              , "User" |> H.text
              ]
            ]
          ]
        ]
      ]
    ]

dialogs : Model -> List (Html Msg)
dialogs model = []
