module GettoUpload.Layout.Page.Side exposing
  ( Model
  , Msg
  , init
  , store
  , storeChanged
  , subscriptions
  , update
  , mobileHeader
  , mobileAddress
  , breadcrumb
  , navHeader
  , navAddress
  , nav
  , navFooter
  )
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Frame.Static as Static
import GettoUpload.Layout.Frame.Credential as Credential
import GettoUpload.Layout.Command.Auth as Auth
import GettoUpload.Layout.Command.Http as Http
import GettoUpload.Layout.Href.Home as Home
import GettoUpload.Layout.Api as Api
import GettoUpload.Layout.View.Menu as Menu exposing ( Menu )
import GettoUpload.Layout.View.Icon as Icon

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode
import Getto.Url.Query.Encode as QueryEncode

import Set exposing ( Set )
import Dict exposing ( Dict )
import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias FrameModel a app appMsg = Frame.Model { a | side : Model } app appMsg
type alias FrameTransition a app appMsg = Transition (FrameModel a app appMsg) Msg
type alias Model =
  { menu       : Menu
  , badgeNames : Dict String String
  , badge      : Http.Entry Badge
  , collapsed  : Set String
  }

type alias Badge =
  { counts : Dict String Int
  }

type Msg
  = BadgeStateChanged (Http.State Badge)
  | MenuOpen  String
  | MenuClose String

init : Frame.InitModel -> ( Model, FrameTransition a app appMsg )
init model =
  ( { menu       = menu
    , badgeNames = badgeNames
    , badge      = Http.empty
    , collapsed  = Set.empty
    }
  , Http.request badge BadgeStateChanged
  )

badge : Http.Request (FrameModel a app appMsg) Badge
badge = Http.get
  { url     = "layout/menu/badge" |> Api.prependRoot
  , headers = Frame.auth >> Auth.credential >> Credential.headers
  , params  = \model -> [] |> QueryEncode.object
  , decoder = Decode.map Badge
      ( Decode.at ["counts"]
        ( Decode.list
          ( Decode.map2 Tuple.pair
            ( Decode.at ["name"]  Decode.string )
            ( Decode.at ["count"] Decode.int )
          )
        |> Decode.map Dict.fromList
        )
      )
  , timeout = 10 * 1000
  , tracker = Nothing
  }

menu : Menu
menu =
  [ ( "main"
    , [ Menu.item (Icon.fas "home") Home.index []
      ]
    )
  , ( "data"
    , [ Menu.item (Icon.fas "home") Home.index []
      ]
    )
  ]

allow : List String -> ( String, List Menu.Item ) -> Bool
allow roles (group,_) =
  (group == "home") ||
  (group == "system") ||
  (roles |> List.member "admin") ||
  (roles |> List.member group)

badgeNames : Dict String String
badgeNames = Dict.fromList
  [ ( Home.index, "home" )
  ]

isCollapsed : String -> Model -> Bool
isCollapsed name model = model.collapsed |> Set.member name

store : Model -> Encode.Value
store model =
  [ ( "collapsed", model.collapsed |> Encode.set Encode.string )
  ] |> Encode.object

storeChanged : Decode.Value -> Model -> Model
storeChanged value model =
  { model
  | collapsed = value |> SafeDecode.at ["collapsed"] (SafeDecode.list (SafeDecode.string "")) |> Set.fromList
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Http.track badge BadgeStateChanged

update : Msg -> Model -> ( Model, FrameTransition a app appMsg )
update msg model =
  case msg of
    BadgeStateChanged state -> ( { model | badge = model.badge |> Http.stateTo state }, Transition.none )

    MenuOpen  name -> ( { model | collapsed = model.collapsed |> Set.remove name }, Transition.none )
    MenuClose name -> ( { model | collapsed = model.collapsed |> Set.insert name }, Transition.none )


mobileHeader : FrameModel a app appMsg -> Html Msg
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

mobileAddress : FrameModel a app appMsg -> Html Msg
mobileAddress = navAddress

breadcrumb : FrameModel a app appMsg -> Html Msg
breadcrumb model =
  H.ul []
    [ H.li [] [ "MAIN" |> H.text ]
    , H.li []
      [ H.a [ A.href Home.index ]
        [ H.i [ A.class "fas fa-home" ] []
        , " " |> H.text
        , "Home" |> H.text
        ]
      ]
    ]

navHeader : FrameModel a app appMsg -> Html Msg
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

navAddress : FrameModel a app appMsg -> Html Msg
navAddress model =
  H.address []
    [ H.a [ A.href Home.index ]
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
      [ H.a [ A.href Home.index ]
        [ H.em [ A.class "badge is-small is-danger" ]
          [ "Error" |> H.text ] -- TODO ここに state の取得ステータスを表示
        ]
      , " " |> H.text
      , H.a [ A.href Home.index ]
        [ H.i [ A.class "fas fa-user-circle" ] []
        , " " |> H.text
        , "manager" |> H.text
        ]
      ]
    ]

nav : FrameModel a app appMsg -> Html Msg
nav model =
  H.section []
    [ H.ul []
      [ H.li []
        [ H.b []
          [ H.a [ A.href "#", E.onClick (MenuClose "MAIN") ]
            [ "MAIN" |> H.text
            , " " |> H.text
            , H.em [ A.class "badge is-danger is-small" ] [ "4" |> H.text ]
            , " " |> H.text
            , H.i [ A.class "fas fa-caret-down" ] []
            ]
          ]
        ]
      , H.li [ A.class "is-active" ]
        [ H.a [ A.href Home.index ]
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
          [ H.a [ A.href "#", E.onClick (MenuClose "DATA") ]
            [ "DATA" |> H.text
            , " " |> H.text
            , H.i [ A.class "fas fa-caret-down" ] []
            ]
          ]
        ]
      , H.li []
        [ H.a [ A.href Home.index ]
          [ H.i [ A.class "fas fa-fw fa-home" ] []
          , " " |> H.text
          , "Home" |> H.text
          ]
        ]
      ]
    ]

navFooter : FrameModel a app appMsg -> Html Msg
navFooter model =
  let
    static = model |> Frame.static
  in
    H.footer [] [ H.p [] [ "version : " ++ (static |> Static.version |> .version) |> H.text ] ]
