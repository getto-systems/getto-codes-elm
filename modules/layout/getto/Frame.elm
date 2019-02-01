module GettoUpload.Layout.Frame exposing
  ( SetupLayout
  , SetupApp
  , InitModel
  , Model
  , Msg
  , init
  , onUrlRequest
  , onUrlChange
  , subscriptions
  , update
  , static
  , auth
  , layout
  , app
  , logout
  , storeLayout
  , storeApp
  , search
  , mapLayout
  , mapApp
  )
import GettoUpload.Layout.Frame.Static     as Static
import GettoUpload.Layout.Frame.Credential as Credential
import GettoUpload.Extension.Command.Auth   as Auth
import GettoUpload.Extension.Command.Store  as Store
import GettoUpload.Extension.Command.Search as Search
import GettoUpload.Version as Version

import Getto.Command as Command
import Getto.Command.Transition as Transition exposing ( Transition )

import Browser
import Browser.Navigation as Navigation
import Url exposing ( Url )
import Json.Decode as Decode
import Html as H exposing ( Html )

type alias Flags =
  { static     : Static.Flags
  , credential : Decode.Value
  , store :
    { layout : Decode.Value
    , app    : Decode.Value
    }
  }

type Model layout app appMsg = Model (Inner layout app appMsg)
type alias Inner layout app appMsg = Base
  { layout : layout
  , app : app
  , search : Search.Model app (Model layout app appMsg) appMsg
  , store  :
    { layout : Store.Model layout
    , app    : Store.Model app
    }
  }

type alias Base a =
  { a
  | static : Static.Model
  , auth   : Auth.Model Credential.Model
  }

type alias InitModel = Base {}

type Msg layoutMsg appMsg
  = UrlRequest Browser.UrlRequest
  | UrlChange Url
  | CredentialChanged Decode.Value
  | LayoutStoreChanged Decode.Value
  | AppStoreChanged Decode.Value
  | Layout layoutMsg
  | App appMsg

onUrlRequest = UrlRequest
onUrlChange  = UrlChange


type alias SetupLayout layout app appMsg layoutMsg =
  { store : Store.Init layout
  , init  : InitModel -> ( layout, Transition (Model layout app appMsg) layoutMsg )
  }
type alias SetupApp layout app appMsg =
  { search : Search.Init app (Model layout app appMsg) appMsg
  , store  : Store.Init app
  , init   : InitModel -> ( app, Transition (Model layout app appMsg) appMsg )
  }

init : SetupLayout layout app appMsg layoutMsg -> SetupApp layout app appMsg -> Flags -> Url -> Navigation.Key -> ( Model layout app appMsg, Cmd (Msg layoutMsg appMsg) )
init setupLayout setupApp flags url key =
  let
    model =
      { static = flags.static     |> Static.init Version.data
      , auth   = flags.credential |> Auth.init Credential.init
      }

    (newLayout,layoutCmd) = model |> setupLayout.init
    (newApp,   appCmd)    = model |> setupApp.init
  in
    Model
      { static = model.static
      , auth   = model.auth
      , layout = newLayout
      , app    = newApp
      , search = Search.init key setupApp.search
      , store  =
        { layout = Store.init Store.Layout setupLayout.store
        , app    = Store.init Store.App    setupApp.store
        }
      }
    |> Command.none
    |> Command.andThen (layoutStoreChanged flags.store.layout >> Command.none)
    |> Command.andThen (appStoreChanged    flags.store.app    >> Command.none)
    |> Command.andThen (searchChanged url)
    |> Command.andThen (Transition.exec layoutCmd >> Command.map Layout)
    |> Command.andThen (Transition.exec appCmd    >> Command.map App)


subscriptions : (layout -> Sub layoutMsg) -> (app -> Sub appMsg) -> Model layout app appMsg -> Sub (Msg layoutMsg appMsg)
subscriptions layoutSubscriptions appSubscriptions (Model model) = Sub.batch
  [ [ CredentialChanged  |> Auth.subscriptions  model.auth
    , LayoutStoreChanged |> Store.subscriptions model.store.layout
    , AppStoreChanged    |> Store.subscriptions model.store.app
    ] |> Sub.batch
  , model.layout |> layoutSubscriptions |> Sub.map Layout
  , model.app    |> appSubscriptions    |> Sub.map App
  ]


type alias LayoutUpdater layout app appMsg layoutMsg = (layoutMsg -> layout -> ( layout, Transition (Model layout app appMsg) layoutMsg ))
type alias AppUpdater layout app appMsg = (appMsg -> app -> ( app, Transition (Model layout app appMsg) appMsg ))

update : LayoutUpdater layout app appMsg layoutMsg -> AppUpdater layout app appMsg -> Msg layoutMsg appMsg  -> Model layout app appMsg -> ( Model layout app appMsg, Cmd (Msg layoutMsg appMsg) )
update layoutUpdater appUpdater message model =
  case message of
    UrlChange url -> model |> searchChanged url
    UrlRequest urlRequest ->
      case urlRequest of
        Browser.Internal url ->  ( model, url  |> Url.toString |> Navigation.load )
        Browser.External href -> ( model, href |> Navigation.load )

    CredentialChanged value -> model |> credentialChanged value |> Command.none

    LayoutStoreChanged value -> model |> layoutStoreChanged value |> Command.none
    AppStoreChanged    value -> model |> appStoreChanged    value |> Command.none

{-
    HttpResponse data result ->
      ( { model | http = model.http |> Http.layoutResponse prop result }, Cmd.none )
-}


    App msg ->
      case model of
        Model m ->
          let
            (newApp,f) = m.app |> appUpdater msg
          in
            Model { m | app = newApp } |> Transition.exec f |> Command.map App

    Layout msg ->
      case model of
        Model m ->
          let
            (newLayout,f) = m.layout |> layoutUpdater msg
          in
            Model { m | layout = newLayout } |> Transition.exec f |> Command.map Layout


credentialChanged : Decode.Value -> Model layout app appMsg -> Model layout app appMsg
credentialChanged value (Model model) =
  Model { model | auth = model.auth |> Auth.changed value }

searchChanged : Url -> Model layout app appMsg -> ( Model layout app appMsg, Cmd (Msg layoutMsg appMsg) )
searchChanged url (Model model) =
  let
    (newApp,f) = model.app |> Search.changed model.search url
  in
    Model { model | app = newApp } |> Transition.exec f |> Command.map App

layoutStoreChanged : Decode.Value -> Model layout app appMsg -> Model layout app appMsg
layoutStoreChanged value (Model model) =
  Model { model | layout = model.layout |> Store.changed model.store.layout value }

appStoreChanged : Decode.Value -> Model layout app appMsg -> Model layout app appMsg
appStoreChanged value (Model model) =
  Model { model | app = model.app |> Store.changed model.store.app value }


static : Model layout app appMsg -> Static.Model
static (Model model) = model.static

auth : Model layout app appMsg -> Auth.Model Credential.Model
auth (Model model) = model.auth

layout : Model layout app appMsg -> layout
layout (Model model) = model.layout

app : Model layout app appMsg -> app
app (Model model) = model.app


logout : Transition (Model layout app appMsg) annonymous
logout (Model model) = model.auth |> Auth.logout

storeLayout : Transition (Model layout app appMsg) annonymous
storeLayout (Model model) = model.layout |> Store.store model.store.layout

storeApp : Transition (Model layout app appMsg) annonymous
storeApp (Model model) = model.app |> Store.store model.store.app

search : Transition (Model layout app appMsg) annonymous
search (Model model) = model.app |> Search.search model.search


mapLayout : Html layoutMsg -> Html (Msg layoutMsg appMsg)
mapLayout = H.map Layout

mapApp : (app -> appMsg) -> List (Html app) -> List (Html (Msg layoutMsg appMsg))
mapApp msg = List.map (H.map (msg >> App))