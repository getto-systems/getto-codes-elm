module GettoUpload.App.Index.Query exposing
  ( Model
  , init
  )

import Getto.Url.Query.Decode as Decode
import Getto.Url.Query.Encode as Encode

type alias Model = ()

init = (decode,encode)

decode : List String -> Model
decode query = ()

encode : Model -> Encode.Value
encode storage = Encode.empty
