module GettoUpload.Layout.Command.Http.Mock exposing
  ( request
  )
import GettoUpload.Layout.Api as Api

import Dict exposing ( Dict )
import Json.Encode as Encode
import Json.Decode as Decode
import Http
import Task

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

request : RequestData data msg -> Cmd msg
request data =
  case mock |> Dict.get (data.method,data.url) of
    Nothing  -> Cmd.none
    Just res ->
      res
      |> Decode.decodeValue data.decoder
      |> Result.mapError (Decode.errorToString >> Http.BadBody)
      |> Task.succeed
      |> Task.perform data.msg

mock : Dict ( String, String ) Decode.Value
mock =
  [ ( ( "GET", "layout/menu/badge" ), Encode.null )
  ]
  |> List.map (\((method,path),res) -> ( ( method, path |> Api.prependRoot ), res ))
  |> Dict.fromList
