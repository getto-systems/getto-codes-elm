module GettoUpload.View.Http exposing
  ( Model
  , Migration
  , State(..)
  , Response
  , ResponseDecoder
  , ResponseValue
  , Progress(..)
  , Error(..)
  , empty
  , clear
  , decoder
  , decodeResponse
  , toResponse
  , load
  , transfer
  , notModified
  , success
  , failure
  , isSuccess
  , isConflict
  , isComplete
  , update
  , state
  , response
  , header
  , body
  , progress
  )
import GettoUpload.View.Http.Progress as Progress

import Getto.Http.Header.Decode as HeaderDecode

import Json.Decode as Decode

type Model response = Model
  { state    : State
  , response : Maybe response
  }

type Migration response
  = Load
  | Transfer Progress
  | NotModified
  | Success response
  | Failure Error

type State
  = Ready (Maybe Error)
  | Connecting (Maybe Progress)

type Response header body = Response
  { header : header
  , body   : body
  }

type alias ResponseDecoder response = ResponseValue -> Result String response
type alias ResponseValue =
  { header : HeaderDecode.Value
  , body   : Decode.Value
  }
type alias ResponseDecoderStruct header body =
  { header : HeaderDecode.Decoder header
  , body   : Decode.Decoder body
  }

type Progress
  = Sending Progress.Model
  | Proccessing
  | Receiving (Maybe Progress.Model)

type Error
  = BadUrl String
  | Timeout
  | NetworkError
  | BadRequest
  | Unauthorized
  | Forbidden
  | NotFound
  | Conflict
  | UnprocessableEntity
  | PreconditionRequired
  | BadStatus Int
  | BadResponse String

empty : Model response
empty = Model
  { state    = Ready Nothing
  , response = Nothing
  }

clear : Model response -> Model response
clear (Model model) = Model { model | response = Nothing }

decoder : ResponseDecoderStruct header body -> ResponseDecoder (Response header body)
decoder decode value =
  case value.header |> HeaderDecode.decode decode.header of
    Err errorHeader ->
      errorHeader
      |> HeaderDecode.errorToString
      |> Debug.log "bad-header"
      |> Err

    Ok responseHeader ->
      case value.body |> Decode.decodeValue decode.body of
        Err errorBody ->
          errorBody
          |> Decode.errorToString
          |> Debug.log "bad-body"
          |> Err

        Ok responseBody ->
          responseBody
          |> toResponse responseHeader
          |> Ok

decodeResponse : ResponseDecoder (Response header body) -> Decode.Decoder (Response header body)
decodeResponse decode =
  ( Decode.map2 ResponseValue
    ( Decode.at ["header"] (Decode.dict Decode.string) )
    ( Decode.at ["body"]    Decode.value )
  )
  |> Decode.andThen
    (\value ->
      case value |> decode of
        Ok  res -> res |> Decode.succeed
        Err err -> err |> Decode.fail
    )

toResponse : header -> body -> Response header body
toResponse responseHeader responseBody = Response
  { header = responseHeader
  , body   = responseBody
  }

load : Migration response
load = Load

transfer : Progress -> Migration response
transfer = Transfer

notModified : Migration response
notModified = NotModified

success : response -> Migration response
success = Success

failure : Error -> Migration response
failure = Failure

isSuccess : Migration response -> Maybe response
isSuccess mig =
  case mig of
    Success res -> Just res
    _ -> Nothing

isComplete : Migration response -> Bool
isComplete mig =
  case mig of
    Success res -> True
    Failure err -> True
    _ -> False

update : Migration response -> Model response -> Model response
update migration (Model model) =
  case migration of
    Load          -> Model { model | state = Nothing   |> Connecting }
    Transfer data -> Model { model | state = Just data |> Connecting }
    NotModified   -> Model { model | state = Nothing  |> Ready }
    Success  res  -> Model { model | state = Nothing  |> Ready, response = Just res }
    Failure  err  -> Model { model | state = Just err |> Ready, response = Nothing }

state : Model response -> State
state (Model model) = model.state

response : Model response -> Maybe response
response (Model model) = model.response

header : Response header body -> header
header (Response res) = res.header

body : Response header body -> body
body (Response res) = res.body

progress : Maybe Progress -> Maybe ( Bool, Int )
progress data =
  case data of
    Just (Sending value)          -> Just ( True,  value |> Progress.percentage )
    Just (Receiving (Just value)) -> Just ( False, value |> Progress.percentage |> (-) 100 )
    _ -> Nothing

isConflict : Model response -> Bool
isConflict = state >> (==) (Ready (Just Conflict))
