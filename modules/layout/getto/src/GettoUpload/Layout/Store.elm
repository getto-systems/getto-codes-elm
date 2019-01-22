module GettoUpload.Layout.Store exposing
  ( Model
  , init
  )
import GettoUpload.Layout.Store.Menu as Menu

import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode
import Json.Decode as Decode

type alias Model =
  { menu : Menu.Model
  }

init = (decode,encode)

encode : Model -> Encode.Value
encode model =
  [ ( "menu", model.menu |> Menu.encode )
  ] |> Encode.object

decode : Decode.Value -> Model
decode value =
  { menu = value |> SafeDecode.valueAt ["menu"] |> Menu.decode
  }
