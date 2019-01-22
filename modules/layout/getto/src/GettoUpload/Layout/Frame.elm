module GettoUpload.Layout.Frame exposing
  ( Msg
  , init
  , onUrlRequest
  , onUrlChange
  , subscriptions
  , update
  , storeLayout
  , storeApp
  , pushUrl
  , mapHtml
  , documentTitle
  , mobileHeader
  , nav
  , navAddress
  , articleHeader
  , articleFooter
  )
import GettoUpload.Layout.Model as Model
import GettoUpload.Layout.Command.Store  as StoreCommand
import GettoUpload.Layout.Command.Search as SearchCommand
import GettoUpload.Layout.Storage as Storage
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

type Msg msg
  = Layout LayoutMsg
  | App msg

type LayoutMsg
  = UrlRequest Browser.UrlRequest
  | UrlChange Url
  | ToggleMenu String

onUrlRequest = UrlRequest >> Layout
onUrlChange  = UrlChange  >> Layout


type alias InitPlugin storage query = ( InitStorage storage, InitQuery query )
type alias InitStorage storage = ( Decode.Value -> storage, storage -> Encode.Value )
type alias InitQuery   query   = ( List String -> query, query -> QueryEncode.Value )
type alias InitModel   storage query model = Model.Init storage query -> ( model, Model.Init storage query )

init : InitPlugin storage query -> InitModel storage query model -> Model.Flags -> Url -> Navigation.Key -> ( Model.Model storage query model, Cmd (Msg msg) )
init (initStorageTuple,initQueryTuple) initApp flags url key =
  let
    (app,model) =
      { static =
        { project = flags.project
        , path    = flags.path
        , key     = key
        }
      , layout =
        { url = url
        , credential = flags.credential
        }
      , plugin =
        { storage = flags |> initStorage initStorageTuple
        , query   = url |> initQuery initQueryTuple
        }
      , command =
        { store  = StoreCommand.init
        , search = SearchCommand.init
        }
      }
      |> initApp
  in
    { static  = model.static
    , layout  = model.layout
    , plugin  = model.plugin
    , command = model.command
    , app     = app
    }
    |> done

initStorage : InitStorage storage -> Model.Flags -> Model.Storage storage
initStorage (decode,encode) flags =
  { layout = flags.storage.layout |> Storage.decode
  , app    = flags.storage.app    |> decode
  , encode = encode
  }

initQuery : InitQuery query -> Url -> Model.Query query
initQuery (decode,encode) url =
  { app    = url.query |> Maybe.map QueryDecode.split |> Maybe.withDefault [] |> decode
  , encode = encode
  }


subscriptions : Model.Model storage query model -> Sub (Msg msg)
subscriptions model = Sub.none


type alias Update storage query model msg = msg -> Model.Model storage query model -> Model.Model storage query model

update : Update storage query model msg -> Msg msg -> Model.Model storage query model -> ( Model.Model storage query model, Cmd (Msg msg) )
update up msg model =
  case msg of
    App sub -> model |> up sub |> done

    Layout (UrlRequest urlRequest) ->
      case urlRequest of
        Browser.Internal url ->  ( model, url  |> Url.toString |> Navigation.load )
        Browser.External href -> ( model, href |> Navigation.load )

    Layout (UrlChange url) ->
      let
        layout = model.layout
      in
        ( { model | layout = { layout | url = url } }
        , Cmd.none
        )

    Layout (ToggleMenu name) -> ( model, Cmd.none )


done : Model.Model storage query model -> ( Model.Model storage query model, Cmd (Msg msg) )
done model =
  ( model, Cmd.none )
  |> andThen storeExec
  |> andThen searchExec

andThen : (model -> ( model, Cmd (Msg msg) )) -> ( model, Cmd (Msg msg) ) -> ( model, Cmd (Msg msg) )
andThen f (model,cmd) =
  let
    (newModel,newCmd) = model |> f
  in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


storeExec : Model.Model storage query model -> ( Model.Model storage query model, Cmd (Msg msg) )
storeExec model =
  let
    command = model.command
    (store,cmd) =
      command.store
      |> StoreCommand.exec
        { layout = model.plugin.storage.layout |> Storage.encode
        , app    = model.plugin.storage.app    |> model.plugin.storage.encode
        }
  in
    ( { model | command = { command | store = store } }
    , cmd
    )

storeCmd : (StoreCommand.Model -> StoreCommand.Model) -> Model.Model storage query model -> Model.Model storage query model
storeCmd f model =
  let
    command = model.command
  in
    { model | command = { command | store = command.store |> f } }

storeLayout : Model.Model storage query model -> Model.Model storage query model
storeLayout = storeCmd StoreCommand.layout

storeApp : Model.Model storage query model -> Model.Model storage query model
storeApp = storeCmd StoreCommand.app


searchExec : Model.Model storage query model -> ( Model.Model storage query model, Cmd (Msg msg) )
searchExec model =
  let
    command = model.command
    (search,cmd) =
      command.search
      |> SearchCommand.exec model.static.key
  in
    ( { model | command = { command | search = search } }
    , cmd
    )

searchCmd : (SearchCommand.Model -> SearchCommand.Model) -> Model.Model storage query model -> Model.Model storage query model
searchCmd f model =
  let
    command = model.command
  in
    { model | command = { command | search = command.search |> f } }

pushUrl : Model.Model storage query model -> Model.Model storage query model
pushUrl model = model |> searchCmd (SearchCommand.pushUrl (model.plugin.query.app |> model.plugin.query.encode))


mapHtml : (sub -> msg) -> List (Html sub) -> List (Html (Msg msg))
mapHtml msg = List.map (H.map (msg >> App))


title : Model.Model storage query model -> String
title model = model.static.path |> I18n.title

documentTitle : Model.Model storage query model -> String
documentTitle model =
  (model |> title)
  ++ " | "
  ++ model.static.project.company
  ++ " "
  ++ model.static.project.title

mobileHeader : Model.Model storage query model -> Html (Msg msg)
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

navHeader : Model.Model storage query model -> Html (Msg msg)
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

nav : Model.Model storage query model -> Html (Msg msg)
nav model =
  H.nav []
    [ model |> navHeader
    , model |> navAddress
    , model |> navBody
    , model |> navFooter
    ]

navAddress : Model.Model storage query model -> Html (Msg msg)
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

navBody : Model.Model storage query model -> Html (Msg msg)
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

navFooter : Model.Model storage query model -> Html (Msg msg)
navFooter model =
  H.footer [] [ H.p [] [ "version : " ++ Version.version |> H.text ] ]

articleHeader : Model.Model storage query model -> Html (Msg msg)
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

articleFooter : Model.Model storage query model -> Html (Msg msg)
articleFooter model =
  H.footer []
    [ H.p []
      [ H.i [ A.class "far fa-copyright" ] []
      , " " |> H.text
      , Version.copyright |> H.text
      ]
    ]
