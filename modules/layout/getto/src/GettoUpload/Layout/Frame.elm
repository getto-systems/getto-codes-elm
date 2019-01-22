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
import GettoUpload.Layout.Frame.Store as FrameStore
import GettoUpload.Layout.Href.Home as HomeHref
import GettoUpload.Layout.Version as Version
import GettoUpload.I18n.App as I18n

import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

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

type alias Base store search a =
  { a
  | static     : Static.Flags
  , credential : Credential.Model
  , store      : Store.Model FrameStore.Model store
  , search     : Search.Model search
  }

type alias Init  store search     = Base store search {}
type alias Model store search app = Base store search { app : app }

type Msg msg
  = Layout LayoutMsg
  | App msg

type LayoutMsg
  = UrlRequest Browser.UrlRequest
  | UrlChange Url
  | LayoutStoreChanged Decode.Value
  | AppStoreChanged Decode.Value
  | CredentialChanged Credential.Flags
  | ToggleMenu String

onUrlRequest = UrlRequest >> Layout
onUrlChange  = UrlChange  >> Layout


type alias InitCommand store search = ( InitStore store, InitSearch search )
type alias InitStore  store = ( Decode.Value -> store, store -> Encode.Value )
type alias InitSearch search = ( List String -> search, search -> QueryEncode.Value )
type alias InitModel store search model = Init store search -> ( model, Init store search )

init : InitCommand store search -> InitModel store search model -> Flags -> Url -> Navigation.Key -> ( Model store search model, Cmd (Msg msg) )
init (initStore,initSearch) initApp flags url key =
  let
    (app,model) =
      { static     = flags.static
      , credential = flags.credential |> Credential.init
      , store      = flags.store |> Store.init (FrameStore.init,initStore)
      , search     = (url,key) |> Search.init initSearch
      }
      |> initApp
  in
    { static     = model.static
    , credential = model.credential
    , store      = model.store
    , search     = model.search
    , app        = app
    }
    |> done


subscriptions : Model store search model -> Sub (Msg msg)
subscriptions model =
  Sub.batch
    [ (LayoutStoreChanged >> Layout,AppStoreChanged >> Layout) |> Store.subscriptions
    , (CredentialChanged >> Layout) |> Credential.subscriptions
    ]


type alias Update store search model msg = msg -> Model store search model -> Model store search model

update : Update store search model msg -> Msg msg -> Model store search model -> ( Model store search model, Cmd (Msg msg) )
update updateApp msg model =
  case msg of
    App sub -> model |> updateApp sub |> done

    Layout (UrlRequest urlRequest) ->
      case urlRequest of
        Browser.Internal url ->  ( model, url  |> Url.toString |> Navigation.load )
        Browser.External href -> ( model, href |> Navigation.load )

    Layout (CredentialChanged flags) ->
      ( { model | credential = model.credential |> Credential.changed flags }, Cmd.none )

    Layout (LayoutStoreChanged value) ->
      ( { model | store = model.store |> Store.layoutChanged value }, Cmd.none )
    Layout (AppStoreChanged value) ->
      ( { model | store = model.store |> Store.appChanged value }, Cmd.none )

    Layout (UrlChange url) ->
      ( { model | search = model.search |> Search.changed url }, Cmd.none )

    Layout (ToggleMenu name) -> ( model, Cmd.none )

done : Model store search model -> ( Model store search model, Cmd (Msg msg) )
done model =
  ( model, Cmd.none )
  |> andThen storeExec
  |> andThen searchExec
  |> andThen credentialExec

andThen : (model -> ( model, Cmd (Msg msg) )) -> ( model, Cmd (Msg msg) ) -> ( model, Cmd (Msg msg) )
andThen f (model,cmd) =
  let
    (newModel,newCmd) = model |> f
  in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


storeExec : Model store search model -> ( Model store search model, Cmd (Msg msg) )
storeExec model =
  let
    (store,cmd) = model.store |> Store.exec
  in
    ( { model | store = store }, cmd )

searchExec : Model store search model -> ( Model store search model, Cmd (Msg msg) )
searchExec model =
  let
    (search,cmd) = model.search |> Search.exec
  in
    ( { model | search = search }, cmd )

credentialExec : Model store search model -> ( Model store search model, Cmd (Msg msg) )
credentialExec model =
  let
    (credential,cmd) = model.credential |> Credential.exec
  in
    ( { model | credential = credential }, cmd )


logout : Model store search model -> Model store search model
logout model = { model | credential = model.credential |> Credential.logout }

updateLayoutStore : (FrameStore.Model -> FrameStore.Model) -> Model store search model -> Model store search model
updateLayoutStore f model = { model | store = model.store |> Store.updateLayout f }

updateAppStore : (store -> store) -> Model store search model -> Model store search model
updateAppStore f model = { model | store = model.store |> Store.updateApp f }

updateQuery : (search -> search) -> Model store search model -> Model store search model
updateQuery f model = { model | search = model.search |> Search.update f }


mapHtml : (sub -> msg) -> List (Html sub) -> List (Html (Msg msg))
mapHtml msg = List.map (H.map (msg >> App))


title : Model store search model -> String
title model = model.static.page.path |> I18n.title

documentTitle : Model store search model -> String
documentTitle model =
  (model |> title)
  ++ " | "
  ++ model.static.project.company
  ++ " "
  ++ model.static.project.title

mobileHeader : Model store search model -> Html (Msg msg)
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

navHeader : Model store search model -> Html (Msg msg)
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

nav : Model store search model -> Html (Msg msg)
nav model =
  H.nav []
    [ model |> navHeader
    , model |> navAddress
    , model |> navBody
    , model |> navFooter
    ]

navAddress : Model store search model -> Html (Msg msg)
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

navBody : Model store search model -> Html (Msg msg)
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

navFooter : Model store search model -> Html (Msg msg)
navFooter model =
  H.footer [] [ H.p [] [ "version : " ++ Version.version |> H.text ] ]

articleHeader : Model store search model -> Html (Msg msg)
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

articleFooter : Model store search model -> Html (Msg msg)
articleFooter model =
  H.footer []
    [ H.p []
      [ H.i [ A.class "far fa-copyright" ] []
      , " " |> H.text
      , Version.copyright |> H.text
      ]
    ]
