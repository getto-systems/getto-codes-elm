module Getto.Http.RequestMock exposing
  ( request
  , track
  )
import Getto.Http

import Http
import Task

request : (Getto.Http.Request -> Maybe Getto.Http.Response) -> (Getto.Http.Response -> msg) -> Getto.Http.Request -> Cmd msg
request f msg req =
  case req |> f of
    Nothing  -> Cmd.none
    Just res -> res |> Task.succeed |> Task.perform msg

track : String -> (Http.Progress -> msg) -> Sub msg
track tracker msg = Sub.none
