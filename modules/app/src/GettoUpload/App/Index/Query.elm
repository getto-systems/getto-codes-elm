module GettoUpload.App.Index.Query exposing
  ( Model
  , init
  )

import Getto.Url.Query.Decode as Decode
import Getto.Url.Query.Encode as Encode

type alias Model = ()

init = (decoder,default,encode)

decoder : Decode.Decoder Model
decoder query = ()

default : () -> Model
default _ = ()

encode : Model -> Encode.Value
encode storage = Encode.empty
