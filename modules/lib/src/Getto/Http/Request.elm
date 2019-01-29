module Getto.Http.Request exposing
  ( request
  , track
  )
import Getto.Http

import Json.Decode as Decode
import Http

request : (Getto.Http.Response -> msg) -> Getto.Http.Request -> Cmd msg
request msg req = Http.request
  { method  = req.method
  , headers = req.headers
  , url     = req.url
  , body    = req.body
  , expect  = Decode.value |> Http.expectJson msg
  , timeout = req.timeout
  , tracker = req.tracker
  }

track : String -> (Http.Progress -> msg) -> Sub msg
track = Http.track
