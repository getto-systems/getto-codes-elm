module GettoUpload.Extension.Command.Http.Mock exposing
  ( request
  )
import GettoUpload.Env.App as Env

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

type Request
  = Real
  | Mock Decode.Value

request : RequestData data msg -> Cmd msg
request data =
  case mock |> Dict.get ( data.method, data.url ) of
    Nothing  -> Cmd.none
    Just res ->
      case res of
        Real ->
          Http.request
            { method  = data.method
            , headers = data.headers
            , url     = data.url
            , body    = data.body
            , expect  = data.decoder |> Http.expectJson data.msg
            , timeout = data.timeout
            , tracker = data.tracker
            }
        Mock val ->
          val
          |> Decode.decodeValue data.decoder
          |> Result.mapError (Decode.errorToString >> Http.BadBody)
          |> Task.succeed
          |> Task.perform data.msg

mock : Dict ( String, String ) Request
mock =
  [ ( ( "GET", "layout/menu/badge" )
    , Mock
      ( [ ( "counts"
          , [ [ ( "name", "home" |> Encode.string )
              , ( "count", 4 |> Encode.int )
              ]
            ] |> Encode.list Encode.object
          )
        ] |> Encode.object
      )
    )
  ]
  |> List.map (\((method,path),res) -> ( ( method, Env.api.upload ++ path ), res ))
  |> Dict.fromList
