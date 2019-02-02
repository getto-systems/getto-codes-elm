module GettoUpload.Extension.Command.Http.Mock exposing
  ( request
  )
import GettoUpload.Extension.Command.Http.Real as Real
import GettoUpload.Env.App as Env

import Dict exposing ( Dict )
import Json.Encode as Encode
import Json.Decode as Decode
import Http
import Task
import Process

type alias RequestData data msg =
  { method  : String
  , headers : List Http.Header
  , url     : String
  , body    : Http.Body
  , decoder : Decode.Decoder data
  , timeout : Maybe Float
  , tracker : Maybe String
  , msg     : Result Http.Error data -> msg
  }

type Request
  = Real
  | Mock Float Decode.Value

request : RequestData data msg -> Cmd msg
request data =
  case mock |> Dict.get ( data.method, data.url ) of
    Just (Mock delay res) ->
      Process.sleep delay
      |> Task.map
        (\_ ->
          res
          |> Decode.decodeValue data.decoder
          |> Result.mapError (Decode.errorToString >> Http.BadBody)
        )
      |> Task.perform data.msg

    _ -> data |> Real.request

mock : Dict ( String, String ) Request
mock =
  [ ( ( "GET", "layout/menu/badge" )
    --, Real
    {--}, Mock 1000
      ( [ ( "counts"
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
