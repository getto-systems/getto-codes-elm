port module GettoUpload.Command.Store exposing
  ( Model
  , Init
  , StoreType(..)
  , init
  , subscriptions
  , store
  , clear
  , changed
  )

import Json.Encode as Encode
import Json.Decode as Decode

port storeLayout : Encode.Value -> Cmd msg
port storeApp    : Encode.Value -> Cmd msg

port onLayoutStoreChanged : (Decode.Value -> msg) -> Sub msg
port onAppStoreChanged    : (Decode.Value -> msg) -> Sub msg

type Model model = Model (Inner model)
type alias Inner model =
  { encode : Encode model
  , decode : Decode model
  , store  : StoreType
  }

type alias Encode model = model -> Encode.Value
type alias Decode model = Decode.Value -> model -> model

type StoreType
  = Layout
  | App

type alias Init model = ( Encode model, Decode model )

init : StoreType -> Init model -> Model model
init storeType (encode,decode) = Model
  { encode = encode
  , decode = decode
  , store  = storeType
  }

subscriptions : Model model -> (Decode.Value -> msg) -> Sub msg
subscriptions (Model model) =
  case model.store of
    Layout -> onLayoutStoreChanged
    App    -> onAppStoreChanged

store : Model model -> model -> Cmd msg
store (Model model) =
  model.encode >>
    case model.store of
      Layout -> storeLayout
      App    -> storeApp

clear : Model model -> Cmd msg
clear (Model model) =
  Encode.null |>
    case model.store of
      Layout -> storeLayout
      App    -> storeApp

changed : Model model -> Decode model
changed (Model model) = model.decode
