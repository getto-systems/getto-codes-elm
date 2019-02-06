module GettoUpload.View.Http exposing
  ( State(..)
  , Response
  , Progress(..)
  , Error(..)
  , response
  , header
  , body
  )

type State header body
  = Empty
  | Loading
  | Progress Progress
  | Success (Response header body)
  | Failure Error

type Response header body = Response
  { header : header
  , body   : body
  }

type Progress
  = Sending
    { sent : Int
    , size : Int
    }
  | Receiving
    { received : Int
    , size : Maybe Int
    }

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
