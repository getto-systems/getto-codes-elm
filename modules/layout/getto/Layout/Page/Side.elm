module GettoUpload.Layout.Page.Side exposing
  ( Msg
  , init
  , encodeStore
  , decodeStore
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
import GettoUpload.Layout.Page.Model as Model
import GettoUpload.Layout.Page.Side.Html as Html
import GettoUpload.Layout.Page.Side.View as View
import GettoUpload.Layout.Frame.Static as Static
import GettoUpload.Layout.Frame.Credential as Credential
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Auth as Auth
import GettoUpload.Command.Http as Http
import GettoUpload.View.Http as HttpView
import GettoUpload.View.Menu as Menu exposing ( Menu )
import GettoUpload.View.Icon as Icon
import GettoUpload.I18n.App  as I18n
import GettoUpload.I18n.Http as HttpI18n
import GettoUpload.Extension.Href as Href exposing ( Href )
import GettoUpload.Extension.Href.Home   as Home
import GettoUpload.Extension.Href.Data.Upload as Upload

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode
import Getto.Url.Query.Encode as QueryEncode

import Json.Encode as Encode
import Json.Decode as Decode

import Set exposing ( Set )
import Dict exposing ( Dict )
import Html as H exposing ( Html )
import Html.Lazy as L

type Msg
  = StateChanged (HttpView.Migration View.Response)
  | MenuOpen  String
  | MenuClose String

signature = "layout-side"

get : Http.Tracker (Model.Frame app) View.Response
get = Http.tracker "get" <|
  \model ->
    Http.get
      { url      = "layout/menu/badge" |> Api.url []
      , headers  = model |> Api.headers
      , params   = QueryEncode.empty
      , response = View.response
      , timeout  = 10 * 1000
      }

getTrack   = Http.track   signature get StateChanged
getRequest = Http.request signature get StateChanged


init : Frame.InitModel -> ( Model.Side, Model.Transition app Msg )
init model =
  ( { menu       = menu
    , badgeNames = badgeNames
    , get        = HttpView.empty
    , collapsed  = Set.empty
    }
  , getRequest
  )

encodeStore : Model.Side -> Encode.Value
encodeStore model =
  [ ( "collapsed", model.collapsed |> Encode.set Encode.string )
  ] |> Encode.object

decodeStore : Decode.Value -> Model.Side -> Model.Side
decodeStore value model =
  { model
  | collapsed = value |> SafeDecode.at ["collapsed"] (SafeDecode.list (SafeDecode.string "")) |> Set.fromList
  }

subscriptions : Model.Side -> Sub Msg
subscriptions model = getTrack

update : Msg -> Model.Side -> ( Model.Side, Model.Transition app Msg )
update msg model =
  case msg of
    StateChanged mig -> ( { model | get = model.get |> HttpView.update mig }, T.none )

    MenuOpen  name -> ( { model | collapsed = model.collapsed |> Set.remove name }, Frame.storeLayout )
    MenuClose name -> ( { model | collapsed = model.collapsed |> Set.insert name }, Frame.storeLayout )


mobileHeader : Model.Frame app -> Html Msg
mobileHeader model = L.lazy
  (\project -> Html.mobileHeader
    { company = project.company
    , title   = project.title
    , sub     = project.sub
    }
  )
  (model |> Frame.static |> Static.project)

navHeader : Model.Frame app -> Html Msg
navHeader model = L.lazy
  (\project -> Html.navHeader
    { company = project.company
    , title   = project.title
    , sub     = project.sub
    }
  )
  (model |> Frame.static |> Static.project)

navFooter : Model.Frame app -> Html Msg
navFooter model = L.lazy
  (\version -> Html.navFooter
    { version = version |> .version
    }
  )
  (model |> Frame.static |> Static.version)

mobileAddress : Model.Frame app -> Html Msg
mobileAddress = navAddress

breadcrumb : Model.Frame app -> Html Msg
breadcrumb model = L.lazy2
  (\static side -> Html.breadcrumb <| View.breadcrumb
    { path = static |> Static.page |> .path
    , menu = side |> .menu
    , i18n = menuI18n
    }
  )
  (model |> Frame.static)
  (model |> Frame.layout |> .side)

navAddress : Model.Frame app -> Html Msg
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
      { get  = side |> .get
      , i18n = HttpI18n.error
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

nav : Model.Frame app -> Html Msg
nav model = L.lazy3
  (\static auth side -> Html.nav
    { open  = MenuOpen
    , close = MenuClose
    , menu  = View.menu
      { path      = model |> Frame.static |> Static.page |> .path
      , roles     = model |> Frame.auth |> Auth.credential |> Credential.roles
      , menu      = side.menu
      , allow     = allow
      , collapsed = side.collapsed |> collapsed
      , badge =
        \href ->
          side.badgeNames
          |> Dict.get (href |> toBadgeName)
          |> Maybe.andThen
            (\name ->
              side.get
              |> HttpView.response
              |> Maybe.andThen (HttpView.body >> .badge >> Dict.get name)
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


menu : Menu
menu =
  [ ( "main"
    , [ Menu.item (Icon.fas "home") Home.index []
      ]
    )
  , ( "upload"
    , [ Menu.item (Icon.fas "file") Upload.list
        [ Menu.item (Icon.edit) Upload.new []
        , Menu.item (Icon.edit) Upload.edit_ []
        ]
      , Menu.item (Icon.fas "file") Upload.list_edit []
      ]
    )
  ]

allow : List String -> ( String, List Menu.Item ) -> Bool
allow roles (group,_) =
  (group == "home") ||
  (group == "system") ||
  (roles |> List.member "admin") ||
  (roles |> List.member group)

collapsed : Set String -> String -> Bool
collapsed data name = data |> Set.member name

badgeNames : Dict String String
badgeNames = Dict.fromList
  [ ( Home.index |> toBadgeName, "home" )
  ]

toBadgeName : Href -> String
toBadgeName href =
  case href |> Href.path of
    Href.Internal path -> "INTERNAL:" ++ path
    Href.Keycloak path -> "KEYCLOAK:" ++ path
