module GettoUpload.Layout.Frame.Page exposing
  ( Model
  , decode
  , path
  )

import Getto.Json.SafeDecode as SafeDecode

import Json.Decode as Decode

type Model = Model Inner
type alias Inner =
  { path : String
  }

decode : Decode.Value -> Model
decode value = Model
  { path = value |> SafeDecode.at ["path"] (SafeDecode.string "")
  }

path : Model -> String
path (Model model) = model.path
