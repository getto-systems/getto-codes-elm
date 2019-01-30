module GettoUpload.Layout.Frame.Credential exposing
  ( Model
  , init
  , token
  , roles
  , headers
  )
import GettoUpload.Layout.Command.Http as Http

import Getto.Json.SafeDecode as SafeDecode

import Json.Decode as Decode

type Model = Model Inner
type alias Inner =
  { token : String
  , roles : List String
  }

init : Decode.Value -> Model
init value = Model
  { token = value |> SafeDecode.at ["token"] (SafeDecode.string "")
  , roles = value |> SafeDecode.at ["roles"] (SafeDecode.list (SafeDecode.string ""))
  }

token : Model -> String
token (Model model) = model.token

roles : Model -> List String
roles (Model model) = model.roles

headers : Model -> List Http.Header
headers (Model model) =
  [ ( "Authorization", model.token )
  ]
