port module GettoUpload.Layout.Command.Store exposing
  ( Flags
  , Model
  , init
  , subscriptions
  , exec
  , layoutChanged
  , appChanged
  , layout
  , app
  , updateLayout
  , updateApp
  )

import Json.Encode as Encode
import Json.Decode as Decode

port storeLayout : Encode.Value -> Cmd msg
port storeApp    : Encode.Value -> Cmd msg

port onLayoutStorageChanged : (Decode.Value -> msg) -> Sub msg
port onAppStorageChanged    : (Decode.Value -> msg) -> Sub msg

type alias Flags =
  { layout : Decode.Value
  , app    : Decode.Value
  }

type Model layout app = Model (Info layout app)

type alias Info layout app =
  { layout : Storage layout
  , app    : Storage app
  }

type alias Storage storage =
  { storage : storage
  , decode  : Decode.Value -> storage
  , encode  : storage -> Encode.Value
  , cmd     : Store
  }

type Store
  = None
  | Store

type alias Init storage = ( Decode.Value -> storage, storage -> Encode.Value )

init : (Init layout, Init app) -> Flags -> Model layout app
init (initLayout,initApp) flags =
  Model
    { layout = flags.layout |> initStorage initLayout
    , app    = flags.app    |> initStorage initApp
    }

initStorage : Init storage -> Decode.Value -> Storage storage
initStorage (decode,encode) value =
  { storage = value |> decode
  , decode  = decode
  , encode  = encode
  , cmd     = None
  }

subscriptions : ( Decode.Value -> msg, Decode.Value -> msg ) -> Sub msg
subscriptions (layoutMsg,appMsg) =
  [ onLayoutStorageChanged layoutMsg
  , onAppStorageChanged    appMsg
  ] |> Sub.batch

exec : Model layout app -> ( Model layout app, Cmd msg )
exec (Model model) =
  let
    (newLayout,layoutCmd) = model.layout |> execCmd storeLayout
    (newApp,   appCmd)    = model.app    |> execCmd storeApp
  in
  ( Model
    { model
    | layout = newLayout
    , app    = newApp
    }
  , Cmd.batch
    [ layoutCmd
    , appCmd
    ]
  )

execCmd : (Encode.Value -> Cmd msg) -> Storage storage -> ( Storage storage, Cmd msg )
execCmd f storage =
  case storage.cmd of
    None  -> ( storage, Cmd.none )
    Store ->
      ( { storage | cmd = None }
      , storage.storage |> storage.encode |> f
      )

layoutChanged : Decode.Value -> Model layout app -> Model layout app
layoutChanged value (Model model) =
  Model { model | layout = model.layout |> storageChanged value }

appChanged : Decode.Value -> Model layout app -> Model layout app
appChanged value (Model model) =
  Model { model | app = model.app |> storageChanged value }

storageChanged : Decode.Value -> Storage storage -> Storage storage
storageChanged value storage =
  { storage | storage = value |> storage.decode }

layout : Model layout app -> Storage layout
layout (Model model) = model.layout

app : Model layout app -> Storage app
app (Model model) = model.app

updateLayout : (layout -> layout) -> Model layout app -> Model layout app
updateLayout f (Model model) =
  Model { model | layout = model.layout |> updateStorage f }

updateApp : (app -> app) -> Model layout app -> Model layout app
updateApp f (Model model) =
  Model { model | app = model.app |> updateStorage f }

updateStorage : (storage -> storage) -> Storage storage -> Storage storage
updateStorage f storage =
  { storage | storage = storage.storage |> f, cmd = Store }
