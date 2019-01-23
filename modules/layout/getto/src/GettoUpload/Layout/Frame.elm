module GettoUpload.Layout.Frame exposing
  ( Init
  , Model
  , Msg
  , init
  , onUrlRequest
  , onUrlChange
  , subscriptions
  , update
  , logout
  , updateLayoutStore
  , updateAppStore
  , updateQuery
  , mapHtml
  , documentTitle
  , mobileHeader
  , nav
  , navAddress
  , articleHeader
  , articleFooter
  )
import GettoUpload.Layout.Command.Static     as Static
import GettoUpload.Layout.Command.Credential as Credential
import GettoUpload.Layout.Command.Store      as Store
import GettoUpload.Layout.Command.Search     as Search
import GettoUpload.Layout.Command.Http       as Http
import GettoUpload.Layout.Frame.Store as FrameStore
import GettoUpload.Layout.Frame.Http  as FrameHttp
import GettoUpload.Layout.Href.Home as HomeHref
import GettoUpload.Layout.Version as Version
import GettoUpload.I18n.App as I18n

import Getto.Prop as Prop exposing ( Prop )
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
  , credential : Credential.Flags
  , store      : Store.Flags
  }

type alias Base search store http a =
  { a
  | static     : Static.Flags
  , credential : Credential.Model
  , search     : Search.Model search
  , store      : Store.Model FrameStore.Model store
  , http       : Http.Model FrameHttp.Model http
  }

type alias Init  search store http     = Base search store http {}
type alias Model search store http app = Base search store http { app : app }

type Msg http msg
  = Layout (LayoutMsg http)
  | App msg

type LayoutMsg http
  = UrlRequest Browser.UrlRequest
  | UrlChange Url
  | CredentialChanged Credential.Flags
  | LayoutStoreChanged Decode.Value
  | AppStoreChanged Decode.Value
  | LayoutHttpResponse (Prop FrameHttp.Model Http.Entry) Http.Response
  | AppHttpResponse (Prop http Http.Entry) Http.Response
  | ToggleMenu String

onUrlRequest = UrlRequest >> Layout
onUrlChange  = UrlChange  >> Layout


type alias InitCommand search store http =
  ( Search.Init search
  , Store.Init store
  , Http.Init http
  )
type alias InitModel search store http model = Init search store http -> ( model, Init search store http )

init : InitCommand search store http -> InitModel search store http model -> Flags -> Url -> Navigation.Key -> ( Model search store http model, Cmd (Msg http msg) )
init (initSearch,initStore,initHttp) initApp flags url key =
  let
    (app,model) =
      { static     = flags.static
      , credential = flags.credential |> Credential.init
      , search     = (url,key) |> Search.init initSearch
      , store      = flags.store |> Store.init (FrameStore.init,initStore)
      , http       = () |> Http.init (FrameHttp.init,initHttp)
      }
      |> initApp
  in
    { static     = model.static
    , credential = model.credential
    , search     = model.search
    , store      = model.store
    , http       = model.http
    , app        = app
    }
    |> done


subscriptions : Model search store http model -> Sub (Msg http msg)
subscriptions model =
  Sub.batch
    [ (LayoutStoreChanged >> Layout,AppStoreChanged >> Layout) |> Store.subscriptions
    , (CredentialChanged >> Layout) |> Credential.subscriptions
    ]


type alias Update search store http model msg = msg -> Model search store http model -> Model search store http model

update : Update search store http model msg -> Msg http msg -> Model search store http model -> ( Model search store http model, Cmd (Msg http msg) )
update updateApp msg model =
  case msg of
    App sub -> model |> updateApp sub |> done

    Layout (UrlRequest urlRequest) ->
      case urlRequest of
        Browser.Internal url ->  ( model, url  |> Url.toString |> Navigation.load )
        Browser.External href -> ( model, href |> Navigation.load )

    Layout (UrlChange url) ->
      ( { model | search = model.search |> Search.changed url }, Cmd.none )

    Layout (CredentialChanged flags) ->
      ( { model | credential = model.credential |> Credential.changed flags }, Cmd.none )

    Layout (LayoutStoreChanged value) ->
      ( { model | store = model.store |> Store.layoutChanged value }, Cmd.none )
    Layout (AppStoreChanged value) ->
      ( { model | store = model.store |> Store.appChanged value }, Cmd.none )

    Layout (LayoutHttpResponse prop result) ->
      ( { model | http = model.http |> Http.layoutResponse prop result }, Cmd.none )
    Layout (AppHttpResponse prop result) ->
      ( { model | http = model.http |> Http.appResponse prop result }, Cmd.none )

    Layout (ToggleMenu name) -> ( model, Cmd.none )

done : Model search store http model -> ( Model search store http model, Cmd (Msg http msg) )
done model =
  ( model, Cmd.none )
  |> andThen credentialExec
  |> andThen searchExec
  |> andThen storeExec
  |> andThen httpExec

andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen f (model,cmd) =
  let
    (newModel,newCmd) = model |> f
  in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


storeExec : Model search store http model -> ( Model search store http model, Cmd (Msg http msg) )
storeExec model =
  let
    (store,cmd) = model.store |> Store.exec
  in
    ( { model | store = store }, cmd )

searchExec : Model search store http model -> ( Model search store http model, Cmd (Msg http msg) )
searchExec model =
  let
    (search,cmd) = model.search |> Search.exec
  in
    ( { model | search = search }, cmd )

credentialExec : Model search store http model -> ( Model search store http model, Cmd (Msg http msg) )
credentialExec model =
  let
    (credential,cmd) = model.credential |> Credential.exec
  in
    ( { model | credential = credential }, cmd )

httpExec : Model search store http model -> ( Model search store http model, Cmd (Msg http msg) )
httpExec model =
  let
    (http,cmd) =
      ( model.http, Cmd.none )
      |> andThen (Http.execLayout LayoutHttpResponse)
      |> andThen (Http.execApp    AppHttpResponse)
  in
    ( { model | http = http }, cmd |> Cmd.map Layout )


logout : Model search store http model -> Model search store http model
logout model = { model | credential = model.credential |> Credential.logout }

updateLayoutStore : (FrameStore.Model -> FrameStore.Model) -> Model search store http model -> Model search store http model
updateLayoutStore f model = { model | store = model.store |> Store.updateLayout f }

updateAppStore : (store -> store) -> Model search store http model -> Model search store http model
updateAppStore f model = { model | store = model.store |> Store.updateApp f }

updateQuery : (search -> search) -> Model search store http model -> Model search store http model
updateQuery f model = { model | search = model.search |> Search.update f }


mapHtml : (sub -> msg) -> List (Html sub) -> List (Html (Msg http msg))
mapHtml msg = List.map (H.map (msg >> App))


title : Model search store http model -> String
title model = model.static.page.path |> I18n.title

documentTitle : Model search store http model -> String
documentTitle model =
  (model |> title)
  ++ " | "
  ++ model.static.project.company
  ++ " "
  ++ model.static.project.title

mobileHeader : Model search store http model -> Html (Msg http msg)
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

navHeader : Model search store http model -> Html (Msg http msg)
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

nav : Model search store http model -> Html (Msg http msg)
nav model =
  H.nav []
    [ model |> navHeader
    , model |> navAddress
    , model |> navBody
    , model |> navFooter
    ]

navAddress : Model search store http model -> Html (Msg http msg)
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

navBody : Model search store http model -> Html (Msg http msg)
navBody model =
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

navFooter : Model search store http model -> Html (Msg http msg)
navFooter model =
  H.footer [] [ H.p [] [ "version : " ++ Version.version |> H.text ] ]

articleHeader : Model search store http model -> Html (Msg http msg)
articleHeader model =
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

articleFooter : Model search store http model -> Html (Msg http msg)
articleFooter model =
  H.footer []
    [ H.p []
      [ H.i [ A.class "far fa-copyright" ] []
      , " " |> H.text
      , Version.copyright |> H.text
      ]
    ]
