module GettoUpload.Layout.Storage exposing
  ( Model
  , encode
  , decode
  )
import GettoUpload.Layout.Storage.Menu as Menu

import Getto.Json.Decode as Decode

import Json.Encode as Encode

type alias Model =
  { menu : Menu.Model
  }

encode : Model -> Encode.Value
encode model =
  [ ( "menu", model.menu |> Menu.encode )
  ] |> Encode.object

decode : Decode.Value -> Model
decode value =
  { menu = value |> Decode.valueAt ["menu"] |> Menu.decode
  }
