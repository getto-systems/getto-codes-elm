module GettoUpload.Layout.Frame.Page exposing
  ( Model
  , Msg
  , setup
  , subscriptions
  , update
  , documentTitle
  , mobileHeader
  , nav
  , navAddress
  , articleHeader
  , articleFooter
  )
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Frame.Static as Static
import GettoUpload.Layout.Command.Store as Store
import GettoUpload.Layout.Href.Home as HomeHref
import GettoUpload.I18n.App as I18n

import Getto.Command.Transition as Transition

import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias FrameModel app appMsg = Frame.Model Model app appMsg
type alias Update     app appMsg = Transition.Update (FrameModel app appMsg) Msg
type Model = Model Inner
type alias Inner =
  { --menu : Menu.Model
  }

type Msg
  = ToggleMenu String

setup : Frame.SetupLayout Model app appMsg Msg
setup =
  { store = store
  , init  = init
  }

init : Frame.InitModel -> ( Model, Update app appMsg )
init model =
  ( Model
    { --menu = Menu.init
    }
  , Transition.none
  )

store : Store.Init Model
store =
  ( \model ->
    [ --( "menu", model.menu |> Menu.store )
    ] |> Encode.object
  , \value model ->
    model
    {-
    Transition.compose Cmd.batch LayoutModel
      (model.menu |> Menu.storeChanged (value |> SafeDecode.valueAt ["menu"]))
    -}
  )

subscriptions : Model -> Sub Msg
subscriptions model =
  [ --model.menu |> Menu.subscriptions |> Sub.map Menu
  ] |> Sub.batch

update : Msg -> Model -> ( Model, Update app appMsg )
update msg model =
  case msg of
    ToggleMenu name -> ( model, Transition.none )


title : Static.Model -> String
title = Static.page >> .path >> I18n.title

documentTitle : FrameModel app appMsg -> String
documentTitle model =
  let
    static = model |> Frame.static
  in
    (static |> title)
    ++ " | "
    ++ (static |> Static.project |> .company)
    ++ " "
    ++ (static |> Static.project |> .title)

mobileHeader : FrameModel app appMsg -> Html Msg
mobileHeader model =
  let
    static = model |> Frame.static
  in
    H.header []
      [ H.p []
        [ H.small [] [ static |> Static.project |> .company |> H.text ]
        , H.wbr [] []
        , " " |> H.text
        , static |> Static.project |> .title |> H.text
        , H.wbr [] []
        , " " |> H.text
        , H.small [] [ H.small [] [ static |> Static.project |> .sub |> H.text ] ]
        ]
      ]

navHeader : FrameModel app appMsg -> Html Msg
navHeader model =
  let
    static = model |> Frame.static
  in
    H.header []
      [ H.p []
        [ H.small [] [ static |> Static.project |> .company |> H.text ]
        , H.br [] []
        , static |> Static.project |> .title |> H.text
        , H.br [] []
        , H.small [] [ H.small [] [ static |> Static.project |> .sub |> H.text ] ]
        ]
      ]

nav : FrameModel app appMsg -> Html Msg
nav model =
  H.nav []
    [ model |> navHeader
    , model |> navAddress
    , model |> navBody
    , model |> navFooter
    ]

navAddress : FrameModel app appMsg -> Html Msg
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

navBody : FrameModel app appMsg -> Html Msg
navBody model =
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

navFooter : FrameModel app appMsg -> Html Msg
navFooter model =
  let
    static = model |> Frame.static
  in
    H.footer [] [ H.p [] [ "version : " ++ (static |> Static.version |> .version) |> H.text ] ]

articleHeader : FrameModel app appMsg -> Html Msg
articleHeader model =
  let
    static = model |> Frame.static
  in
    H.header []
      [ H.h1 [] [ static |> title |> H.text ]
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

articleFooter : FrameModel app appMsg -> Html Msg
articleFooter model =
  let
    static = model |> Frame.static
  in
    H.footer []
      [ H.p []
        [ H.i [ A.class "far fa-copyright" ] []
        , " " |> H.text
        , static |> Static.version |> .copyright |> H.text
        ]
      ]
