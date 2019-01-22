port module GettoUpload.Layout.Command.Store exposing
  ( Model
  , init
  , layout
  , app
  , exec
  )

import Json.Encode as Encode

port storeLayout : Encode.Value -> Cmd msg
port storeApp    : Encode.Value -> Cmd msg

type alias Model = List StorageType

type alias Storage =
  { layout : Encode.Value
  , app    : Encode.Value
  }

type StorageType
  = Layout
  | App

init : Model
init = []

layout : Model -> Model
layout model =
  if model |> List.member Layout
    then model
    else Layout :: model

app : Model -> Model
app model =
  if model |> List.member App
    then model
    else App :: model

exec : Storage -> Model -> ( Model, Cmd msg )
exec storage model =
  ( []
  , model
    |> List.map (execCmd storage)
    |> Cmd.batch
  )

execCmd : Storage -> StorageType -> Cmd msg
execCmd storage t =
  case t of
    Layout -> storage.layout |> storeLayout
    App    -> storage.app    |> storeApp
