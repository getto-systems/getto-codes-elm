module GettoUpload.App.Index.Storage exposing
  ( Model
  , init
  )

import Getto.Json.Decode as Decode

import Json.Encode as Encode

type alias Model = ()

init = (decode,encode)

decode : Decode.Value -> Model
decode value = ()

encode : Model -> Encode.Value
encode storage = Encode.null
