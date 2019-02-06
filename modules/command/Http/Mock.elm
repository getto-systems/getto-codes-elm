module GettoUpload.Command.Http.Mock exposing
  ( request
  )
import GettoUpload.Command.Http.Real as Real
import GettoUpload.View.Http as HttpView
import GettoUpload.Env.App as Env

import Getto.Http.Header.Decode as HeaderDecode

import Dict exposing ( Dict )
import Json.Encode as Encode
import Json.Decode as Decode
import Http
import Task
import Process

type alias RequestData header body msg =
  { method   : String
  , headers  : List Http.Header
  , url      : String
  , body     : Http.Body
  , response : Response header body
  , timeout  : Maybe Float
  , tracker  : Maybe String
  , msg      : HttpView.State header body -> msg
  }

type alias Response header body =
  { header : HeaderDecode.Decoder header
  , body   : Decode.Decoder body
  }

type Request
  = Real
  | Mock Float HeaderDecode.Value Decode.Value

request : RequestData header body msg -> Cmd msg
request data =
  case mock |> Dict.get ( data.method, data.url ) of
    Just (Mock delay rawHeader rawBody) ->
      [ delay * 1 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Sending { sent = 1, size = 4 } |> HttpView.Progress)
      , delay * 2 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Sending { sent = 2, size = 4 } |> HttpView.Progress)
      , delay * 3 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Sending { sent = 3, size = 4 } |> HttpView.Progress)
      , delay * 4 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Sending { sent = 4, size = 4 } |> HttpView.Progress)
      , delay * 5 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Receiving { received = 1, size = Just 4 } |> HttpView.Progress)
      , delay * 6 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Receiving { received = 2, size = Just 4 } |> HttpView.Progress)
      , delay * 7 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Receiving { received = 3, size = Just 4 } |> HttpView.Progress)

      , delay
        |> Process.sleep
        |> Task.map
          (\_ ->
            case rawHeader |> HeaderDecode.decode data.response.header of
              Err headerError -> headerError |> HeaderDecode.errorToString |> HttpView.BadHeader |> HttpView.Failure
              Ok header ->
                case rawBody |> Decode.decodeValue data.response.body of
                  Err bodyError -> bodyError |> Decode.errorToString >> HttpView.BadBody |> HttpView.Failure

                  Ok body -> body |> HttpView.response header |> HttpView.Success
          )

      ] |> List.map (Task.perform data.msg) |> Cmd.batch

    _ -> data |> Real.request

mock : Dict ( String, String ) Request
mock =
  [ ( ( "GET", "layout/menu/badge" )
    --, Real
    {--}, Mock 1000
      ( [
        ] |> Dict.fromList
      )
      ( [ ( "badge"
          , [ [ ( "name", "home" |> Encode.string )
              , ( "count", 4 |> Encode.int )
              ]
            ] |> Encode.list Encode.object
          )
        ] |> Encode.object
      )
    --}
    )
  ]
  |> List.map (\((method,path),res) -> ( ( method, Env.api.host ++ path ), res ))
  |> Dict.fromList
