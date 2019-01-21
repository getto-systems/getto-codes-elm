module GettoUpload.App.Index.Storage exposing
  ( Model
  , init
  )

import Json.Decode as Decode
import Json.Encode as Encode

type alias Model = ()

init = (decoder,default,encode)

decoder : Decode.Decoder Model
decoder = Decode.succeed ()

default : () -> Model
default _ = ()

encode : Model -> Encode.Value
encode storage = Encode.null
