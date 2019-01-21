module GettoUpload.Layout.Storage exposing
  ( Model
  , default
  , encode
  , decoder
  )
import GettoUpload.Layout.Storage.Menu as Menu

import Json.Encode as Encode
import Json.Decode as Decode

type alias Model =
  { menu : Menu.Model
  }

default : () -> Model
default _ =
  { menu = Menu.init
  }

encode : Model -> Encode.Value
encode model =
  [ ( "menu", model.menu |> Menu.encode )
  ] |> Encode.object

decoder : Decode.Decoder Model
decoder = Decode.map Model
  (Decode.at ["menu"] Menu.decoder)
