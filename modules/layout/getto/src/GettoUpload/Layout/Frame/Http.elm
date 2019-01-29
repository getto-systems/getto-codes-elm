module GettoUpload.Layout.Frame.Http exposing
  ( Model
  , Data
  , init
  )
import GettoUpload.Layout.Frame.Http.Data as Data
import GettoUpload.Layout.Frame.Menu.Http as Menu
import GettoUpload.Layout.Command.Http as Http
import GettoUpload.Layout.Command.Http.Entry as Entry

import Json.Decode as Decode

type Model = Model Inner
type alias Inner =
  { state : Http.Model Data.Model State
  }

type alias State = Entry.Model Data.Model (List StateRequest)
  { menu : Menu.Model
  }

type StateRequest
  = Menu

init : Http.Init Data.Model Model
init =
  ( Model
    { state = Entry.init initState
    }
  , exec
  )

exec : Data.Model -> Model -> ( Model, Request Model )
exec data (Model model) =
  (model,[])
  |> Http.exec .state (\f m -> { m | state = m.state |> f }) (Entry.exec data)
  |> Tuple.mapFirst Model

initState = (stateApi,execState,stateDecoder)

stateApi : Http.Api
stateApi = "state" |> Api.prependRoot |> Http.get
  { timeout = 30 * 1000
  , tracker = Nothing
  }

execState : Data.Model -> List StateRequest -> Http.Query
execState data =
  List.map
    (\request ->
      case request of
        Menu -> ( "menu", "true" |> QueryEncode.string )
    )
  >> QueryEncode.object
  >> Http.query []

{-
state : Model -> State.Model
state = Http.response state_

menu : Model -> Model
menu = Http.request state_ State.requestMenu

state : Decode.Decoder State
state = Decode.map State
  (Decode.at ["menu"] Menu.decoder)
-}
