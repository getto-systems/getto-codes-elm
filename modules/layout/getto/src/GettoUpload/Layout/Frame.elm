module GettoUpload.Layout.Frame exposing
  ( InitModel
  , Model
  , Msg(..)
  , init
  , onUrlRequest
  , onUrlChange
  , subscriptions
  , update
  , layout
  , app
  , updateApp
  , logout
  , storeLayout
  , storeApp
  , search
  , mapHtml
  , documentTitle
  , mobileHeader
  , nav
  , navAddress
  , articleHeader
  , articleFooter
  )
import GettoUpload.Layout.Command.Auth   as Auth
import GettoUpload.Layout.Command.Store  as Store
import GettoUpload.Layout.Command.Search as Search
--import GettoUpload.Layout.Command.Http   as Http
import GettoUpload.Layout.Frame.Static     as Static
import GettoUpload.Layout.Frame.Credential as Credential
import GettoUpload.Layout.Href.Home as HomeHref
import GettoUpload.Layout.Version as Version
import GettoUpload.I18n.App as I18n

import Getto.Command.Transit as Transit
import Getto.Url.Query.Encode as QueryEncode

import Browser
import Browser.Navigation as Navigation
import Url exposing ( Url )
import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias Flags =
  { static     : Static.Flags
  , credential : Decode.Value
  , store :
    { layout : Decode.Value
    , app    : Decode.Value
    }
  }

type Model model msg = Model (Inner model msg)
type alias Inner model msg = Base
  { layout : LayoutModel
  , app : model
  , search : Search.Model model msg
  , store  :
    { layout : Store.Model LayoutModel
    , app    : Store.Model model
    }
  }

type alias Base a =
  { a
  | static : Static.Model
  , auth   : Auth.Model Credential.Model
  }

type alias InitModel = Base {}

type alias LayoutModel =
  { --menu : Menu.Model
  }

type Msg msg
  = UrlRequest Browser.UrlRequest
  | UrlChange Url
  | CredentialChanged Decode.Value
  | LayoutStoreChanged Decode.Value
  | AppStoreChanged Decode.Value
  --| HttpProgress Data Http.Progress
  --| HttpResponse Data Http.Response
  | Layout LayoutMsg
  | App msg

type LayoutMsg
  = ToggleMenu String

onUrlRequest = UrlRequest
onUrlChange  = UrlChange


type alias InitCommand model msg =
  ( Search.Init model msg
  , Store.Init  model
  )
type alias Construct model = InitModel -> model
type alias Init model msg = Model model msg -> ( Model model msg, Cmd msg )

init : InitCommand model msg -> Construct model -> Init model msg -> Flags -> Url -> Navigation.Key -> ( Model model msg, Cmd (Msg msg) )
init (appSearch,appStore) constructApp initApp flags url key =
  let
    model =
      { static = flags.static     |> Static.init Version.data
      , auth   = flags.credential |> Auth.init Credential.init
      }

    newLayout = model |> constructLayout
    newApp    = model |> constructApp
  in
    Model
      { static = model.static
      , auth   = model.auth
      , layout = newLayout
      , app    = newApp
      , search = Search.init key appSearch
      , store  =
        { layout = Store.init Store.Layout layoutStore
        , app    = Store.init Store.App    appStore
        }
      }
    |> Transit.none
    |> Transit.andThen (layoutStoreChanged flags.store.layout >> Transit.none)
    |> Transit.andThen (appStoreChanged    flags.store.app    >> Transit.none)
    |> Transit.andThen (searchChanged url)
    --|> Transit.andThen (initLayout >> Transit.map Layout)
    |> Transit.andThen (initApp >> Transit.map App)

constructLayout : Construct LayoutModel
constructLayout model =
  { --menu = Menu.init
  }

layoutStore : Store.Init LayoutModel
layoutStore =
  ( \model ->
    [ --( "menu", model.menu |> Menu.store )
    ] |> Encode.object
  , \value model ->
    model
    {-
    Transit.compose LayoutModel
      (model.menu |> Menu.storeChanged (value |> SafeDecode.valueAt ["menu"]))
    -}
  )


subscriptions : (Model model msg -> Sub msg) -> Model model msg -> Sub (Msg msg)
subscriptions sub (Model model) = Sub.batch
  [ [ CredentialChanged  |> Auth.subscriptions  model.auth
    , LayoutStoreChanged |> Store.subscriptions model.store.layout
    , AppStoreChanged    |> Store.subscriptions model.store.app
    ] |> Sub.batch
  , Model model |> sub |> Sub.map App
  ]


update : (msg -> Model model msg -> ( Model model msg, Cmd msg )) -> Msg msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
update appUpdater msg model =
  case msg of
    UrlChange url -> model |> searchChanged url
    UrlRequest urlRequest ->
      case urlRequest of
        Browser.Internal url ->  ( model, url  |> Url.toString |> Navigation.load )
        Browser.External href -> ( model, href |> Navigation.load )

    CredentialChanged value -> model |> credentialChanged value |> Transit.none

    LayoutStoreChanged value -> model |> layoutStoreChanged value |> Transit.none
    AppStoreChanged    value -> model |> appStoreChanged    value |> Transit.none

{-
    HttpResponse data result ->
      ( { model | http = model.http |> Http.layoutResponse prop result }, Cmd.none )
-}


    App    sub -> model |> appUpdater   sub |> Transit.map App
    Layout sub -> model |> updateLayout sub |> Transit.map Layout

updateLayout : LayoutMsg -> Model model msg -> ( Model model msg, Cmd LayoutMsg )
updateLayout msg model =
  case msg of
    ToggleMenu name -> ( model, Cmd.none )

credentialChanged : Decode.Value -> Model model msg -> Model model msg
credentialChanged value (Model model) =
  Model { model | auth = model.auth |> Auth.changed value }

layoutStoreChanged : Decode.Value -> Model model msg -> Model model msg
layoutStoreChanged value (Model model) =
  Model { model | layout = model.layout |> Store.changed model.store.layout value }

appStoreChanged : Decode.Value -> Model model msg -> Model model msg
appStoreChanged value (Model model) =
  Model { model | app = model.app |> Store.changed model.store.app value }


layout : Model model msg -> LayoutModel
layout (Model model) = model.layout

app : Model model msg -> model
app (Model model) = model.app

updateApp : (model -> ( model, Cmd msg )) -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
updateApp f (Model model) =
  let
    (newApp,cmd) = model.app |> f
  in
    ( Model { model | app = newApp }, cmd |> Cmd.map App )


logout : Model model msg -> ( Model model msg, Cmd (Msg msg) )
logout (Model model) = ( Model model, model.auth |> Auth.logout |> Cmd.map Layout )

storeLayout : Model model msg -> ( Model model msg, Cmd (Msg msg) )
storeLayout (Model model) = ( Model model, model.layout |> Store.store model.store.layout |> Cmd.map Layout )

storeApp : Model model msg -> ( Model model msg, Cmd (Msg msg) )
storeApp (Model model) = ( Model model, model.app |> Store.store model.store.app |> Cmd.map Layout )

search : Model model msg -> ( Model model msg, Cmd (Msg msg) )
search (Model model) = ( Model model, model.app |> Search.search model.search |> Cmd.map App )

searchChanged : Url -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
searchChanged url (Model model) =
  let
    (newApp,cmd) = model.app |> Search.changed model.search url
  in
    ( Model { model | app = newApp }
    , cmd |> Cmd.map App
    )


mapHtml : (sub -> msg) -> List (Html sub) -> List (Html (Msg msg))
mapHtml msg = List.map (H.map (msg >> App))


title : Inner model msg -> String
title model = model.static |> Static.page |> .path |> I18n.title

documentTitle : Model model msg -> String
documentTitle (Model model) =
  (model |> title)
  ++ " | "
  ++ (model.static |> Static.project |> .company)
  ++ " "
  ++ (model.static |> Static.project |> .title)

mobileHeader : Model model msg -> Html (Msg msg)
mobileHeader (Model model) =
  H.header []
    [ H.p []
      [ H.small [] [ model.static |> Static.project |> .company |> H.text ]
      , H.wbr [] []
      , " " |> H.text
      , model.static |> Static.project |> .title |> H.text
      , H.wbr [] []
      , " " |> H.text
      , H.small [] [ H.small [] [ model.static |> Static.project |> .sub |> H.text ] ]
      ]
    ]

navHeader : Model model msg -> Html (Msg msg)
navHeader (Model model) =
  H.header []
    [ H.p []
      [ H.small [] [ model.static |> Static.project |> .company |> H.text ]
      , H.br [] []
      , model.static |> Static.project |> .title |> H.text
      , H.br [] []
      , H.small [] [ H.small [] [ model.static |> Static.project |> .sub |> H.text ] ]
      ]
    ]

nav : Model model msg -> Html (Msg msg)
nav model =
  H.nav []
    [ model |> navHeader
    , model |> navAddress
    , model |> navBody
    , model |> navFooter
    ]

navAddress : Model model msg -> Html (Msg msg)
navAddress (Model model) =
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

navBody : Model model msg -> Html (Msg msg)
navBody (Model model) =
  H.section []
    [ H.ul []
      [ H.li []
        [ H.b []
          [ H.a [ A.href "#", E.onClick (ToggleMenu "MAIN" |> Layout) ]
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
          [ H.a [ A.href "#", E.onClick (ToggleMenu "DATA" |> Layout) ]
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

navFooter : Model model msg -> Html (Msg msg)
navFooter (Model model) =
  let
    data = Version.data
  in
    H.footer [] [ H.p [] [ "version : " ++ data.version |> H.text ] ]

articleHeader : Model model msg -> Html (Msg msg)
articleHeader (Model model) =
  H.header []
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

articleFooter : Model model msg -> Html (Msg msg)
articleFooter (Model model) =
  let
    data = Version.data
  in
    H.footer []
      [ H.p []
        [ H.i [ A.class "far fa-copyright" ] []
        , " " |> H.text
        , data.copyright |> H.text
        ]
      ]
