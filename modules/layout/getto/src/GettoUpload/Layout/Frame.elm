module GettoUpload.Layout.Frame exposing
  ( Flags
  , Init
  , Model
  , Msg
  , init
  , onUrlRequest
  , onUrlChange
  , subscriptions
  , update
  , storeLayout
  , storePage
  , pushUrl
  , mapHtml
  , title
  , documentTitle
  , mobileHeader
  , nav
  , navAddress
  , articleHeader
  , articleFooter
  )
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


type alias Flags =
  { project    : Project
  , path       : String
  , credential : Credential
  , storage :
    { layout : Decode.Value
    , page   : Decode.Value
    }
  }

type alias Project =
  { name    : String
  , company : String
  , title   : String
  , sub     : String
  }

type alias Credential =
  { token : String
  , roles : List String
  }

type alias Init storage query =
  { static : Static
  , layout : Layout
  , plugin : Plugin storage query
  , command :
    { store  : StoreCommand.Model
    , search : SearchCommand.Model
    }
  }

type alias Model storage query page =
  { static : Static
  , layout : Layout
  , plugin : Plugin storage query
  , page   : page
  , command :
    { store  : StoreCommand.Model
    , search : SearchCommand.Model
    }
  }

type alias Static =
  { project : Project
  , path    : String
  , key     : Navigation.Key
  }

type alias Layout =
  { url        : Url
  , credential : Credential
  }

type alias Plugin storage query =
  { storage : Storage storage
  , query   : Query query
  }

type alias Storage storage =
  { layout : Storage.Model
  , page   : storage
  , encode : storage -> Encode.Value
  }

type alias Query query =
  { page   : query
  , encode : query -> QueryEncode.Value
  }


type Msg msg
  = Frame FrameMsg
  | Page msg

type FrameMsg
  = UrlRequest Browser.UrlRequest
  | UrlChange Url
  | ToggleMenu String

onUrlRequest = UrlRequest >> Frame
onUrlChange  = UrlChange  >> Frame


type alias InitPlugin storage query = ( InitStorage storage, InitQuery query )
type alias InitStorage storage = ( Decode.Value -> storage, storage -> Encode.Value )
type alias InitQuery   query   = ( List String -> query, query -> QueryEncode.Value )
type alias InitModel   storage query model = Init storage query -> ( model, Init storage query )

init : InitPlugin storage query -> InitModel storage query model -> Flags -> Url -> Navigation.Key -> ( Model storage query model, Cmd (Msg msg) )
init (initStorageTuple,initQueryTuple) initPage flags url key =
  let
    (page,model) =
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
      |> initPage
  in
    { static  = model.static
    , layout  = model.layout
    , plugin  = model.plugin
    , command = model.command
    , page    = page
    }
    |> done

initStorage : InitStorage storage -> Flags -> Storage storage
initStorage (decode,encode) flags =
  { layout = flags.storage.layout |> Storage.decode
  , page   = flags.storage.page   |> decode
  , encode = encode
  }

initQuery : InitQuery query -> Url -> Query query
initQuery (decode,encode) url =
  { page   = url.query |> Maybe.map QueryDecode.split |> Maybe.withDefault [] |> decode
  , encode = encode
  }


subscriptions : Model storage query model -> Sub (Msg msg)
subscriptions model = Sub.none


type alias Update storage query model msg = msg -> Model storage query model -> Model storage query model

update : Update storage query model msg -> Msg msg -> Model storage query model -> ( Model storage query model, Cmd (Msg msg) )
update up msg model =
  case msg of
    Page sub -> model |> up sub |> done

    Frame (UrlRequest urlRequest) ->
      case urlRequest of
        Browser.Internal url ->  ( model, url  |> Url.toString |> Navigation.load )
        Browser.External href -> ( model, href |> Navigation.load )

    Frame (UrlChange url) ->
      let
        layout = model.layout
      in
        ( { model | layout = { layout | url = url } }
        , Cmd.none
        )

    Frame (ToggleMenu name) -> ( model, Cmd.none )


done : Model storage query model -> ( Model storage query model, Cmd (Msg msg) )
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


storeExec : Model storage query model -> ( Model storage query model, Cmd (Msg msg) )
storeExec model =
  let
    command = model.command
    (store,cmd) =
      command.store
      |> StoreCommand.exec
        { layout = model.plugin.storage.layout |> Storage.encode
        , page   = model.plugin.storage.page   |> model.plugin.storage.encode
        }
  in
    ( { model | command = { command | store = store } }
    , cmd
    )

storeCmd : (StoreCommand.Model -> StoreCommand.Model) -> Model storage query model -> Model storage query model
storeCmd f model =
  let
    command = model.command
  in
    { model | command = { command | store = command.store |> f } }

storeLayout : Model storage query model -> Model storage query model
storeLayout = storeCmd StoreCommand.layout

storePage : Model storage query model -> Model storage query model
storePage = storeCmd StoreCommand.page


searchExec : Model storage query model -> ( Model storage query model, Cmd (Msg msg) )
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

searchCmd : (SearchCommand.Model -> SearchCommand.Model) -> Model storage query model -> Model storage query model
searchCmd f model =
  let
    command = model.command
  in
    { model | command = { command | search = command.search |> f } }

pushUrl : Model storage query model -> Model storage query model
pushUrl model = model |> searchCmd (SearchCommand.pushUrl (model.plugin.query.page |> model.plugin.query.encode))


mapHtml : (sub -> msg) -> List (Html sub) -> List (Html (Msg msg))
mapHtml msg = List.map (H.map (msg >> Page))


title : Model storage query model -> String
title model = model.static.path |> I18n.title

documentTitle : Model storage query model -> String
documentTitle model =
  (model |> title)
  ++ " | "
  ++ model.static.project.company
  ++ " "
  ++ model.static.project.title

mobileHeader : Model storage query model -> Html (Msg msg)
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

navHeader : Model storage query model -> Html (Msg msg)
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

nav : Model storage query model -> Html (Msg msg)
nav model =
  H.nav []
    [ model |> navHeader
    , model |> navAddress
    , model |> navBody
    , model |> navFooter
    ]

navAddress : Model storage query model -> Html (Msg msg)
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

navBody : Model storage query model -> Html (Msg msg)
navBody model =
  H.section []
    [ H.ul []
      [ H.li []
        [ H.b []
          [ H.a [ A.href "#", E.onClick (ToggleMenu "MAIN" |> Frame) ]
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
          [ H.a [ A.href "#", E.onClick (ToggleMenu "DATA" |> Frame) ]
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

navFooter : Model storage query model -> Html (Msg msg)
navFooter model =
  H.footer [] [ H.p [] [ "version : " ++ Version.version |> H.text ] ]

articleHeader : Model storage query model -> Html (Msg msg)
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

articleFooter : Model storage query model -> Html (Msg msg)
articleFooter model =
  H.footer []
    [ H.p []
      [ H.i [ A.class "far fa-copyright" ] []
      , " " |> H.text
      , Version.copyright |> H.text
      ]
    ]
