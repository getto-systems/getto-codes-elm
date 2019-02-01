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
import GettoUpload.Layout.Page.Side.View as View
import GettoUpload.Layout.Page.Side.Html as Html
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Frame.Static as Static
import GettoUpload.Layout.Frame.Credential as Credential
import GettoUpload.Layout.Api as Api
import GettoUpload.Extension.Command.Auth as Auth
import GettoUpload.Extension.Command.Http as Http
import GettoUpload.Extension.Href as Href exposing ( Href )
import GettoUpload.Extension.Href.Home as Home
import GettoUpload.Extension.View.Http as HttpView
import GettoUpload.Extension.View.Menu as Menu exposing ( Menu )
import GettoUpload.Extension.View.Icon as Icon
import GettoUpload.I18n.App  as I18n
import GettoUpload.I18n.Http as HttpI18n

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode
import Getto.Url.Query.Encode as QueryEncode

import Set exposing ( Set )
import Dict exposing ( Dict )
import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Lazy as L

type alias FrameModel a app appMsg = Frame.Model { a | side : Model } app appMsg
type alias FrameTransition a app appMsg = Transition (FrameModel a app appMsg) Msg
type alias Model =
  { menu       : Menu
  , badge      : Http.Entry Badge
  , collapsed  : Set String
  }

type alias Badge =
  { counts : Dict String Int
  }

type Msg
  = BadgeStateChanged (HttpView.State Badge)
  | MenuOpen  String
  | MenuClose String

init : Frame.InitModel -> ( Model, FrameTransition a app appMsg )
init model =
  ( { menu      = menu
    , badge     = Http.empty
    , collapsed = Set.empty
    }
  , Http.request badge BadgeStateChanged
  )

badge : Http.Request (FrameModel a app appMsg) Badge
badge = "layout/menu/badge" |> Api.upload
  (\(url,headers) ->
    Http.get
      { url     = url
      , headers = headers
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
      }
  )

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
  [ ( Home.index |> toBadgeName, "home" )
  ]

toBadgeName : Href -> String
toBadgeName href =
  case href |> Href.path of
    Href.Internal path -> "INTERNAL:" ++ path
    Href.Keycloak path -> "KEYCLOAK:" ++ path

collapsed : Set String -> String -> Bool
collapsed data name = data |> Set.member name

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

    MenuOpen  name -> ( { model | collapsed = model.collapsed |> Set.remove name }, Frame.storeLayout )
    MenuClose name -> ( { model | collapsed = model.collapsed |> Set.insert name }, Frame.storeLayout )


mobileHeader : FrameModel a app appMsg -> Html Msg
mobileHeader model = L.lazy
  (\project -> Html.mobileHeader
    { company = project.company
    , title   = project.title
    , sub     = project.sub
    }
  )
  (model |> Frame.static |> Static.project)

navHeader : FrameModel a app appMsg -> Html Msg
navHeader model = L.lazy
  (\project -> Html.navHeader
    { company = project.company
    , title   = project.title
    , sub     = project.sub
    }
  )
  (model |> Frame.static |> Static.project)

navFooter : FrameModel a app appMsg -> Html Msg
navFooter model = L.lazy
  (\version -> Html.navFooter
    { version = version |> .version
    }
  )
  (model |> Frame.static |> Static.version)

mobileAddress : FrameModel a app appMsg -> Html Msg
mobileAddress = navAddress

breadcrumb : FrameModel a app appMsg -> Html Msg
breadcrumb model = L.lazy2
  (\static side -> Html.breadcrumb <| View.breadcrumb
    { path = static |> Static.page |> .path
    , menu = side |> .menu
    , i18n = menuI18n
    }
  )
  (model |> Frame.static)
  (model |> Frame.layout |> .side)

navAddress : FrameModel a app appMsg -> Html Msg
navAddress model = L.lazy2
  (\auth side -> Html.navAddress
    { title = "Upload"
    , mode1 =
      { title = "mode1"
      , state = True
      }
    , mode2 =
      { title = "mode2"
      , state = False
      }
    , badge = View.badgeState
      { state = side |> .badge |> Http.state
      , i18n  = HttpI18n.error
      }
    , roles = auth |> Auth.credential |> Credential.roles
    , href =
      { config = Home.index
      , profile = Home.index
      }
    , i18n =
      { title = identity
      , mode  = identity
      , role  = I18n.role
      }
    }
  )
  (model |> Frame.auth)
  (model |> Frame.layout |> .side)

nav : FrameModel a app appMsg -> Html Msg
nav model = L.lazy3
  (\static auth side -> Html.nav
    { open  = MenuOpen
    , close = MenuClose
    , menu  = View.menu
      { path      = model |> Frame.static |> Static.page |> .path
      , roles     = model |> Frame.auth |> Auth.credential |> Credential.roles
      , menu      = side |> .menu
      , allow     = allow
      , collapsed = side |> .collapsed |> collapsed
      , badge =
        \href ->
          badgeNames
          |> Dict.get (href |> toBadgeName)
          |> Maybe.andThen
            (\name ->
              side |> .badge
              |> Http.response
              |> Maybe.andThen (.counts >> Dict.get name)
            )
      , i18n = menuI18n
      }
    }
  )
  (model |> Frame.static)
  (model |> Frame.auth)
  (model |> Frame.layout |> .side)

menuI18n : View.MenuI18n
menuI18n =
  { title = I18n.title
  , menu  = I18n.menu
  }
