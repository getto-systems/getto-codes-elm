module GettoUpload.Layout.Frame.Menu.Http exposing
  ( Model
  , decoder
  )

import Json.Decode as Decode

type alias Model = {}

decoder : Decode.Decoder Model
decoder = Decode.succeed {}
