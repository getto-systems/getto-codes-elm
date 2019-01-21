module GettoUpload.Layout.Frame exposing
  ( Flags
  , Model
  , Static
  , Layout
  , Plugin
  , init
  , storeLayout
  , storePage
  )
import GettoUpload.Layout.Command.Store  as StoreCommand
import GettoUpload.Layout.Command.Search as SearchCommand
import GettoUpload.Layout.Storage as Storage

import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

import Browser.Navigation as Navigation
import Url exposing ( Url )
import Json.Encode as Encode
import Json.Decode as Decode

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

type alias InitStorage storage = ( Decode.Decoder storage, () -> storage, storage -> Encode.Value )
type alias InitQuery   query   = ( QueryDecode.Decoder query, () -> query, query -> QueryEncode.Value )
type alias InitModel   storage query model = ( Static, Layout, Plugin storage query ) -> model
type alias InitCommand storage query model = Model storage query model -> Model storage query model

init : InitStorage storage -> InitQuery query -> InitModel storage query model -> InitCommand storage query model -> Flags -> Url -> Navigation.Key -> ( Model storage query model, Cmd msg )
init (storageDecoder,defaultStorage,encodeStorage) (queryDecoder,defaultQuery,encodeQuery) page command flags url key =
  let
    static =
      { project = flags.project
      , path    = flags.path
      , key     = key
      }
    layout =
      { url = url
      , credential = flags.credential
      }
    plugin =
      { storage =
        { layout = flags.storage.layout |> decodeStorage Storage.decoder Storage.default
        , page   = flags.storage.page   |> decodeStorage storageDecoder  defaultStorage
        , encode = encodeStorage
        }
      , query =
        { page   = url.query |> Maybe.map (QueryDecode.split queryDecoder) |> withDefault defaultQuery
        , encode = encodeQuery
        }
      }
  in
    { static = static
    , layout = layout
    , plugin = plugin
    , page   = (static,layout,plugin) |> page
    , command =
      { store  = StoreCommand.init
      , search = SearchCommand.init
      }
    }
    |> command
    |> done

decodeStorage : Decode.Decoder a -> (() -> a) -> Decode.Value -> a
decodeStorage decoder default =
  Decode.decodeValue decoder
  >> Result.toMaybe
  >> withDefault default

withDefault : (() -> a) -> Maybe a -> a
withDefault default raw =
  case raw of
    Just value -> value
    Nothing    -> default ()

done : Model storage query model -> ( Model storage query model, Cmd msg )
done model =
  ( model, Cmd.none )
  |> andThen storeExec
  |> andThen searchExec

andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen f (model,cmd) =
  let
    (newModel,newCmd) = model |> f
  in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


storeExec : Model storage query model -> ( Model storage query model, Cmd msg )
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


searchExec : Model storage query model -> ( Model storage query model, Cmd msg )
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
