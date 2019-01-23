module GettoUpload.Layout.Frame.Http exposing
  ( Model
  , State
  , init
  , state
  )
import GettoUpload.Layout.Frame.Menu.Http as Menu
import GettoUpload.Layout.Command.Http as Http

import Getto.Prop as Prop exposing ( Prop )

import Json.Decode as Decode

type alias Model =
  { state : Http.Entry
  }

type alias State =
  { menu : Menu.Model
  }

init : Http.Init Model
init =
  ( { state = Http.entry
    }
  , [ Prop.create .state (\f m -> { m | state = m.state |> f })
    ]
  )

state : Decode.Decoder State
state = Decode.map State
  (Decode.at ["menu"] Menu.decoder)
