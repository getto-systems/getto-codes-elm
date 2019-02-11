module GettoUpload.View.Http exposing
  ( State(..)
  , Response
  , Progress(..)
  , Error(..)
  , response
  , header
  , body
  , progress
  )
import GettoUpload.View.Http.Progress as Progress

type State header body
  = Ready (Maybe (Result Error (Response header body)))
  | Connecting (Maybe Progress)

type Response header body = Response
  { header : header
  , body   : body
  }

type Progress
  = Sending Progress.Model
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

response : header -> body -> Response header body
response headerData bodyData = Response { header = headerData, body = bodyData }

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
