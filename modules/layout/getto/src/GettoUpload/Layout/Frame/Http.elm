module GettoUpload.Layout.Frame.Http exposing
  ( Model
  , init
  )
import GettoUpload.Layout.Command.Http as Http

import Getto.Prop as Prop exposing ( Prop )

type alias Model = {}

init : Http.Init Model
init =
  ( {}
  , []
  )
