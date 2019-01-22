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

port onLayoutStoreChanged : (Decode.Value -> msg) -> Sub msg
port onAppStoreChanged    : (Decode.Value -> msg) -> Sub msg

type alias Flags =
  { layout : Decode.Value
  , app    : Decode.Value
  }

type Model layout app = Model (Info layout app)

type alias Info layout app =
  { layout : Entry layout
  , app    : Entry app
  }

type alias Entry store =
  { store  : store
  , decode : Decode.Value -> store
  , encode : store -> Encode.Value
  , cmd    : Store
  }

type Store
  = None
  | Store

type alias Init store = ( Decode.Value -> store, store -> Encode.Value )

init : (Init layout, Init app) -> Flags -> Model layout app
init (initLayout,initApp) flags =
  Model
    { layout = flags.layout |> initStore initLayout
    , app    = flags.app    |> initStore initApp
    }

initStore : Init store -> Decode.Value -> Entry store
initStore (decode,encode) value =
  { store  = value |> decode
  , decode = decode
  , encode = encode
  , cmd    = None
  }

subscriptions : ( Decode.Value -> msg, Decode.Value -> msg ) -> Sub msg
subscriptions (layoutMsg,appMsg) =
  [ onLayoutStoreChanged layoutMsg
  , onAppStoreChanged    appMsg
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

execCmd : (Encode.Value -> Cmd msg) -> Entry store -> ( Entry store, Cmd msg )
execCmd f store =
  case store.cmd of
    None  -> ( store, Cmd.none )
    Store ->
      ( { store | cmd = None }
      , store.store |> store.encode |> f
      )

layoutChanged : Decode.Value -> Model layout app -> Model layout app
layoutChanged value (Model model) =
  Model { model | layout = model.layout |> storageChanged value }

appChanged : Decode.Value -> Model layout app -> Model layout app
appChanged value (Model model) =
  Model { model | app = model.app |> storageChanged value }

storageChanged : Decode.Value -> Entry store -> Entry store
storageChanged value store =
  { store | store = value |> store.decode }

layout : Model layout app -> Entry layout
layout (Model model) = model.layout

app : Model layout app -> Entry app
app (Model model) = model.app

updateLayout : (layout -> layout) -> Model layout app -> Model layout app
updateLayout f (Model model) =
  Model { model | layout = model.layout |> updateStore f }

updateApp : (app -> app) -> Model layout app -> Model layout app
updateApp f (Model model) =
  Model { model | app = model.app |> updateStore f }

updateStore : (store -> store) -> Entry store -> Entry store
updateStore f store =
  { store | store = store.store |> f, cmd = Store }
