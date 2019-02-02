port module GettoUpload.Command.Auth exposing
  ( Model
  , Init
  , init
  , subscriptions
  , changed
  , logout
  , credential
  )

import Json.Decode as Decode

port clearCredential : () -> Cmd msg

port onCredentialChanged : (Decode.Value -> msg) -> Sub msg

type Model model = Model (Inner model)
type alias Inner model =
  { credential : model
  , decode     : Decode model
  }

type alias Decode model = Decode.Value -> model

type alias Init model = ( Decode model )

init : Init model -> Decode.Value -> Model model
init (decode) data = Model
  { credential = data |> decode
  , decode     = decode
  }

subscriptions : Model model -> (Decode.Value -> msg) -> Sub msg
subscriptions _ = onCredentialChanged

changed : Decode.Value -> Model model -> Model model
changed data (Model model) =
  Model { model | credential = data |> model.decode }

logout : Model model -> Cmd msg
logout _ = clearCredential ()

credential : Model model -> model
credential (Model model) = model.credential
