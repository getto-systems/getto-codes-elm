module GettoUpload.View.Http exposing
  ( Model
  , Migration(..)
  , State(..)
  , Response
  , Progress(..)
  , Error(..)
  , empty
  , clear
  , response
  , update
  , state
  , header
  , body
  , progress
  )
import GettoUpload.View.Http.Progress as Progress

type Model header body = Model
  { state    : State
  , response : Maybe (Response header body)
  }

type Migration header body
  = Load
  | Transfer Progress
  | Success (Response header body)
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
  | UnprocessableEntity
  | BadStatus Int
  | BadHeader String
  | BadBody String

empty : Model header body
empty = Model
  { state    = Ready Nothing
  , response = Nothing
  }

clear : Model header body -> Model header body
clear (Model model) = Model { model | response = Nothing }

response : header -> body -> Response header body
response headerData bodyData = Response
  { header = headerData
  , body   = bodyData
  }

update : Migration header body -> Model header body -> Model header body
update migration (Model model) =
  case migration of
    Load          -> Model { model | state = Nothing   |> Connecting }
    Transfer data -> Model { model | state = Just data |> Connecting }
    Success  res  -> Model { model | state = Nothing  |> Ready, response = Just res }
    Failure  err  -> Model { model | state = Just err |> Ready, response = Nothing }

state : Model header body -> State
state (Model model) = model.state

header : Response header body -> header
header (Response res) = res.header

body : Model header body -> Maybe body
body (Model model) =
  model.response |> Maybe.map (\(Response res) -> res.body)

progress : Maybe Progress -> Maybe ( Bool, Int )
progress data =
  case data of
    Just (Sending value)          -> Just ( True,  value |> Progress.percentage )
    Just (Receiving (Just value)) -> Just ( False, value |> Progress.percentage |> (-) 100 )
    _ -> Nothing
