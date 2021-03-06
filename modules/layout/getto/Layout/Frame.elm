module GettoCodes.Layout.Frame exposing
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
  , clearLayout
  , clearApp
  , pushUrl
  , loadUrl
  , fixedMidashi
  , mapLayout
  , mapApp
  )
import GettoCodes.Layout.Frame.Static     as Static
import GettoCodes.Layout.Frame.Credential as Credential
import GettoCodes.Command.Auth         as Auth
import GettoCodes.Command.Store        as Store
import GettoCodes.Command.Search       as Search
import GettoCodes.Command.FixedMidashi as FixedMidashi
import GettoCodes.View.Href exposing ( Href )
import GettoCodes.Version as Version

import Getto.Command as Command
import Getto.Command.Transition as T exposing ( Transition )

import Json.Decode as Decode

import Browser
import Browser.Navigation as Navigation
import Url exposing ( Url )
import Html as H exposing ( Html )

type alias Flags =
  { static     : Static.Flags
  , credential : Decode.Value
  , store :
    { layout : Decode.Value
    , app    : Decode.Value
    }
  }

type Model layout app = Model (Inner layout app)
type alias Inner layout app = Base
  { layout : layout
  , app    : app
  , store  :
    { layout : Store.Model layout
    , app    : Store.Model app
    }
  , search : Search.Model app
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
  | Layout layoutMsg
  | App appMsg

onUrlRequest = UrlRequest
onUrlChange  = UrlChange


type alias SetupLayout layout app layoutMsg =
  { store : Store.Init layout
  , init  : InitModel -> ( layout, Transition (Model layout app) layoutMsg )
  }
type alias SetupApp layout app appMsg =
  { store  : Store.Init app
  , search : Search.Init app
  , init   : InitModel -> ( app, Transition (Model layout app) appMsg )
  }

init : SetupLayout layout app layoutMsg -> SetupApp layout app appMsg -> Flags -> Url -> Navigation.Key -> ( Model layout app, Cmd (Msg layoutMsg appMsg) )
init setupLayout setupApp flags url key =
  let
    model =
      { static = flags.static     |> Static.init Version.data
      , auth   = flags.credential |> Auth.init Credential.init
      }

    (newLayout,layoutCmd) = model |> setupLayout.init
    (newApp,   appCmd)    = model |> setupApp.init

    search = Search.init key setupApp.search

    store =
      { layout = Store.init Store.Layout setupLayout.store
      , app    = Store.init Store.App    setupApp.store
      }
  in
    Model
      { static = model.static
      , auth   = model.auth
      , layout = newLayout                          |> Store.decode store.layout flags.store.layout
      , app    = newApp |> Search.decode search url |> Store.decode store.app    flags.store.app
      , store  = store
      , search = search
      }
    |> Command.none
    |> Command.andThen (T.exec layoutCmd >> Command.map Layout)
    |> Command.andThen (T.exec appCmd    >> Command.map App)


subscriptions : (layout -> Sub layoutMsg) -> (app -> Sub appMsg) -> Model layout app -> Sub (Msg layoutMsg appMsg)
subscriptions layoutSubscriptions appSubscriptions (Model model) = Sub.batch
  [ CredentialChanged |> Auth.subscriptions  model.auth
  , model.layout |> layoutSubscriptions |> Sub.map Layout
  , model.app    |> appSubscriptions    |> Sub.map App
  ]


type alias LayoutUpdater layout app layoutMsg = (layoutMsg -> layout -> ( layout, Transition (Model layout app) layoutMsg ))
type alias AppUpdater layout app appMsg = (appMsg -> app -> ( app, Transition (Model layout app) appMsg ))

update : LayoutUpdater layout app layoutMsg -> AppUpdater layout app appMsg -> Msg layoutMsg appMsg  -> Model layout app -> ( Model layout app, Cmd (Msg layoutMsg appMsg) )
update layoutUpdater appUpdater message model =
  case message of
    UrlChange url -> ( model, Cmd.none )
    UrlRequest urlRequest ->
      case urlRequest of
        Browser.Internal url ->  ( model, url  |> Url.toString |> Navigation.load )
        Browser.External href -> ( model, href |> Navigation.load )

    CredentialChanged value -> model |> credentialChanged value |> Command.none

    App msg ->
      case model of
        Model m ->
          let
            (newApp,f) = m.app |> appUpdater msg
          in
            Model { m | app = newApp } |> T.exec f |> Command.map App

    Layout msg ->
      case model of
        Model m ->
          let
            (newLayout,f) = m.layout |> layoutUpdater msg
          in
            Model { m | layout = newLayout } |> T.exec f |> Command.map Layout


credentialChanged : Decode.Value -> Model layout app -> Model layout app
credentialChanged value (Model model) =
  Model { model | auth = model.auth |> Auth.changed value }


static : Model layout app -> Static.Model
static (Model model) = model.static

auth : Model layout app -> Auth.Model Credential.Model
auth (Model model) = model.auth

layout : Model layout app -> layout
layout (Model model) = model.layout

app : Model layout app -> app
app (Model model) = model.app


logout : Transition (Model layout app) annonymous
logout (Model model) = model.auth |> Auth.logout

storeLayout : Transition (Model layout app) annonymous
storeLayout (Model model) = model.layout |> Store.store model.store.layout

storeApp : Transition (Model layout app) annonymous
storeApp (Model model) = model.app |> Store.store model.store.app

clearLayout : Transition (Model layout app) annonymous
clearLayout (Model model) = Store.clear model.store.layout

clearApp : Transition (Model layout app) annonymous
clearApp (Model model) = Store.clear model.store.app

pushUrl : Transition (Model layout app) annonymous
pushUrl (Model model) = model.app |> Search.pushUrl model.search

loadUrl : Href -> Transition (Model layout app) annonymous
loadUrl = Search.loadUrl >> always

fixedMidashi : Transition (Model layout app) annonymous
fixedMidashi = FixedMidashi.create |> always


mapLayout : Html layoutMsg -> Html (Msg layoutMsg appMsg)
mapLayout = H.map Layout

mapApp : (app -> appMsg) -> List (Html app) -> List (Html (Msg layoutMsg appMsg))
mapApp msg = List.map (H.map (msg >> App))
