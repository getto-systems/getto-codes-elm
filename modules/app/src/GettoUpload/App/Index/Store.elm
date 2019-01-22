module GettoUpload.App.Index.Store exposing
  ( Model
  , init
  )

import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode
import Json.Decode as Decode

type alias Model = ()

init = (decode,encode)

decode : Decode.Value -> Model
decode value = ()

encode : Model -> Encode.Value
encode storage = Encode.null
