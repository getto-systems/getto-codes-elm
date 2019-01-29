module GettoUpload.Layout.Frame.State exposing
  ( Model
  , init
  , exec
  )
import GettoUpload.Layout.Frame.State.Menu as Menu

type alias Model =
  { menu : Menu.Model
  }

init = (decoder,body)

decoder : Decode.Value -> Model
decoder = Decode.succeed {}

body : Model -> Http.Body
body model = Http.emptyBody

exec : Data.Model -> Model -> ( Model, Http.Request )
exec data model = ( model, Http.None )
