module GettoUpload.View.Http exposing
  ( Model
  , Migration
  , State(..)
  , Response
  , Progress(..)
  , Error(..)
  , empty
  , clear
  , toResponse
  , load
  , transfer
  , notModified
  , success
  , failure
  , isSuccess
  , update
  , state
  , response
  , header
  , body
  , progress
  )
import GettoUpload.View.Http.Progress as Progress

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
  | BadHeader String
  | BadBody String

empty : Model response
empty = Model
  { state    = Ready Nothing
  , response = Nothing
  }

clear : Model response -> Model response
clear (Model model) = Model { model | response = Nothing }

toResponse : header -> body -> Response header body
toResponse headerData bodyData = Response
  { header = headerData
  , body   = bodyData
  }

load : Migration response
load = Load

transfer : Progress -> Migration response
transfer = Transfer

notModified : Migration response
notModified = NotModified

success : Response header body -> Migration (Response header body)
success = Success

failure : Error -> Migration response
failure = Failure

isSuccess : Migration (Response header body) -> Maybe (Response header body)
isSuccess mig =
  case mig of
    Success res -> Just res
    _ -> Nothing

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

response : Model (Response header body) -> Maybe (Response header body)
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
