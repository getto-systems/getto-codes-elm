module GettoUpload.Extension.View.Http exposing
  ( State(..)
  , Progress(..)
  , Error(..)
  )

type State data
  = Empty
  | Loading
  | Progress Progress
  | Success data
  | Failure Error

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
  | BadStatus Int
  | BadBody String
