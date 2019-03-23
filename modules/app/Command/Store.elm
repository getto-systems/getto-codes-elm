port module GettoCodes.Command.Store exposing
  ( Model
  , Init
  , StoreType(..)
  , init
  , store
  , clear
  , decode
  )

import Json.Encode as Encode
import Json.Decode as Decode

port storeLayout : Encode.Value -> Cmd msg
port storeApp    : Encode.Value -> Cmd msg

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
init storeType (encoder,decoder) = Model
  { encode = encoder
  , decode = decoder
  , store  = storeType
  }

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

decode : Model model -> Decode model
decode (Model model) = model.decode
