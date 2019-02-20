module GettoUpload.Command.Http.Mock exposing
  ( request
  )
import GettoUpload.Command.Http.Real as Real
import GettoUpload.View.Http as HttpView
import GettoUpload.Env as Env

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
  , msg      : HttpView.Migration header body -> msg
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
  case mock |> Dict.get ( data.method, data.url |> String.split "?" |> List.head |> Maybe.withDefault "" ) of
    Just (Mock delay rawHeader rawBody) ->
      [ delay * 1 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Sending { current = 1, size = 4 } |> HttpView.transfer)
      , delay * 2 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Sending { current = 2, size = 4 } |> HttpView.transfer)
      , delay * 3 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Sending { current = 3, size = 4 } |> HttpView.transfer)
      , delay * 4 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Proccessing |> HttpView.transfer)
      , delay * 5 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Receiving (Just { current = 1, size = 4 }) |> HttpView.transfer)
      , delay * 6 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Receiving (Just { current = 2, size = 4 }) |> HttpView.transfer)
      , delay * 7 / 8
        |> Process.sleep
        |> Task.map (\_ -> HttpView.Receiving (Just { current = 3, size = 4 }) |> HttpView.transfer)

      , delay
        |> Process.sleep
        |> Task.map
          (\_ ->
            case rawHeader |> HeaderDecode.decode data.response.header of
              Err headerError ->
                headerError |> HeaderDecode.errorToString |> HttpView.BadHeader |> HttpView.failure

              Ok header ->
                case rawBody |> Decode.decodeValue data.response.body of
                  Err bodyError ->
                    bodyError |> Decode.errorToString >> HttpView.BadBody |> HttpView.failure

                  Ok body ->
                    body |> HttpView.toResponse header |> HttpView.success
          )

      ] |> List.map (Task.perform data.msg) |> Cmd.batch

    _ -> data |> Real.request

mock : Dict ( String, String ) Request
mock =
  [ ( ( "GET", "layout/menu/badge" )
    {--, Real --}
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
  , ( ( "POST", "upload" )
    {--}, Real --}
    {--, Mock 3000
      ( [ ( "x-upload-id", "3" )
        ] |> Dict.fromList
      )
      ( Encode.null
      )
    --}
    )
  , ( ( "GET", "upload/4" )
    {--, Real --}
    {--}, Mock 1000
      ( [
        ] |> Dict.fromList
      )
      ( Encode.object
        [ ( "info"
          , [ ( "name",  "John Doe" |> Encode.string )
            , ( "memo",  "hello, world\nmulti line memo" |> Encode.string )
            , ( "age",   40 |> Encode.int )
            , ( "email", "john@example.com" |> Encode.string )
            , ( "tel",   "090-xxxx-1234" |> Encode.string )
            ] |> Encode.object
          )
        , ( "detail"
          , [ ( "birthday", "1980-10-10" |> Encode.string )
            , ( "start_at", "10:10" |> Encode.string )
            , ( "gender",   "male" |> Encode.string )
            , ( "quality",  "high" |> Encode.string )
            , ( "roles",    ["upload"] |> Encode.list Encode.string )
            ] |> Encode.object
          )
        ]
      )
    --}
    )
  , ( ( "PUT", "upload/4/info" )
    {--, Real --}
    {--}, Mock 1000
      ( [
        ] |> Dict.fromList
      )
      ( Encode.object
        [ ( "info"
          , [ ( "name",  "John Doe - UPDATED!!" |> Encode.string )
            , ( "memo",  "hello, world\nmulti line memo" |> Encode.string )
            , ( "age",   40 |> Encode.int )
            , ( "email", "john@example.com" |> Encode.string )
            , ( "tel",   "090-xxxx-1234" |> Encode.string )
            ] |> Encode.object
          )
        , ( "detail"
          , [ ( "birthday", "1980-10-10" |> Encode.string )
            , ( "start_at", "10:10" |> Encode.string )
            , ( "gender",   "male" |> Encode.string )
            , ( "quality",  "high" |> Encode.string )
            , ( "roles",    ["upload"] |> Encode.list Encode.string )
            ] |> Encode.object
          )
        ]
      )
    --}
    )
  , ( ( "GET", "uploads" )
    {--, Real --}
    {--}, Mock 1000
      ( [ ( "x-paging-max", "10" )
        ] |> Dict.fromList
      )
      ( Encode.list Encode.object
        [ [ ( "id",     1        |> Encode.int )
          , ( "name",   "text-1" |> Encode.string )
          , ( "gender", "male"   |> Encode.string )
          ]
        , [ ( "id",     2        |> Encode.int )
          , ( "name",   "text-2" |> Encode.string )
          , ( "gender", "female" |> Encode.string )
          ]
        , [ ( "id",     3        |> Encode.int )
          , ( "name",   "text-3" |> Encode.string )
          , ( "gender", "other"  |> Encode.string )
          ]
        ]
      )
    --}
    )
  ]
  |> List.map (\((method,path),res) -> ( ( method, Env.api.host ++ path ), res ))
  |> Dict.fromList
