module GettoUpload.Layout.Command.Http.Entry exposing
  ( Model
  , Init
  , State(..)
  , init
  , exec
  , tracker
  , raw
  , state
  , updateState
  , request
  , progress
  , response
  )

import Getto.Http

import Json.Decode as Decode
import Http

type Model data request response = Model (Inner data request response)
type alias Inner data request response =
  { exec    : Exec data request
  , tracker : Maybe String
  , state   : State Decode.Value
  , decoder : Decode.Decoder response
  , request : Maybe request
  }

type alias Exec data request = data -> request -> Request

type alias Request =
  { method  : String
  , headers : List Http.Header
  , url     : String
  , body    : Http.Body
  , timeout : Maybe Float
  }

type State a
  = Empty
  | Loading (Maybe Http.Progress)
  | Success a
  | Failure Http.Error

type alias Init data request response =
  { exec    : Exec data request
  , tracker : Maybe String
  , decoder : Decode.Decoder response
  }

init : Init data request response -> Model data request response
init data = Model
  { exec    = data.exec
  , tracker = data.tracker
  , state   = Empty
  , decoder = data.decoder
  , request = Nothing
  }

exec : data -> Model data request response -> ( Model data request response, Maybe Getto.Http.Request )
exec data (Model model) =
  ( Model { model | request = Nothing, state = Loading Nothing }
  , model.request |> Maybe.map (model.exec data >> toRequest model)
  )

toRequest : Model data request response -> Request -> Getto.Http.Request
toRequest model req =
  { method  = req.method
  , headers = req.headers
  , url     = req.url
  , body    = req.body
  , timeout = req.timeout
  , tracker = model.tracker
  }

tracker : Model data request response -> Maybe String
tracker (Model model) = model.tracker

raw : Model data request response -> State Decode.Value
raw (Model model) = model.state

state : Model data request response -> State response
state (Model model) =
  case model.state of
    Empty         -> Empty
    Loading value -> value |> Loading
    Failure error -> error |> Failure
    Success value ->
      case value |> Decode.decodeValue model.decoder of
        Ok  val   -> val   |> Success
        Err error -> error |> Decode.errorToString |> Http.BadBody |> Failure

updateState : (State Decode.Value -> State Decode.Value) -> Model data request response -> Model data request response
updateState f (Model model) = Model { model | state = model.state |> f }

request : request -> Model data request response -> Model data request response
request req (Model model) = Model { model | request = Just req }

progress : Http.Progress -> Model data request response -> Model data request response
progress value (Model model) = Model { model | state = Loading (Just value) }

response : Getto.Http.Response -> Model data request response -> Model data request response
response result (Model model) =
  case result of
    Ok  res   -> Model { model | state = Success res }
    Err error -> Model { model | state = Failure error }
