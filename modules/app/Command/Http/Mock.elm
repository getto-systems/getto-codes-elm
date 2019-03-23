module GettoCodes.Command.Http.Mock exposing
  ( request
  )
import GettoCodes.Command.Http.Real as Real
import GettoCodes.View.Http as HttpView
import GettoCodes.Env as Env

import Dict exposing ( Dict )
import Json.Encode as Encode
import Http
import Task
import Process

type alias RequestData response msg =
  { method   : String
  , headers  : List Http.Header
  , url      : String
  , body     : Http.Body
  , response : HttpView.ResponseDecoder response
  , timeout  : Maybe Float
  , tracker  : Maybe String
  , msg      : HttpView.Migration response -> msg
  }

type Request
  = Real
  | Mock Float HttpView.ResponseValue

request : RequestData response msg -> Cmd msg
request data =
  let
    sending current = Process.sleep >> Task.map
      (\_ -> HttpView.Sending { current = current, size = 4 } |> HttpView.transfer)

    proccessing = Process.sleep >> Task.map
      (\_ -> HttpView.Proccessing |> HttpView.transfer)

    receiving current = Process.sleep >> Task.map
      (\_ -> HttpView.Receiving (Just { current = current, size = 4 }) |> HttpView.transfer)

    pathInfo =
      data.url
      |> String.replace Env.api.host ""
      |> String.split "?" |> List.head |> Maybe.withDefault ""
      |> String.split "/"

    entry = mock |> List.filterMap
      (\((method,mockPath),res) ->
        if method /= data.method
          then Nothing
          else
            let
              mockInfo = mockPath |> String.split "/"
            in
              if (pathInfo |> List.length) /= (mockInfo |> List.length)
                then Nothing
                else
                  let
                    match = List.map2 Tuple.pair pathInfo mockInfo |> List.all
                      (\(pathEntry,mockEntry) ->
                        if mockEntry == ":id"
                          then (pathEntry |> String.toInt) /= Nothing
                          else pathEntry == mockEntry
                      )
                  in
                    if match
                      then Just res
                      else Nothing
      )
      |> List.head
  in
    case entry of
      Just (Mock delay value) ->
        [ delay * 1 / 8 |> sending 1
        , delay * 2 / 8 |> sending 2
        , delay * 3 / 8 |> sending 3
        , delay * 4 / 8 |> proccessing
        , delay * 5 / 8 |> receiving 1
        , delay * 6 / 8 |> receiving 2
        , delay * 7 / 8 |> receiving 3

        , delay
          |> Process.sleep
          |> Task.map
            (\_ ->
              case value |> data.response of
                Err error -> error |> HttpView.BadResponse |> HttpView.failure
                Ok  res   -> res   |> HttpView.success
            )

        ] |> List.map (Task.perform data.msg) |> Cmd.batch

      _ -> data |> Real.request

mock : List ( ( String, String ), Request )
mock =
  [ ( ( "GET", "layout/menu/badge" )
    {--, Real --}
    {--}, Mock 1000
      { header =
        [
        ] |> Dict.fromList
      , body =
        [ [ ( "name", "home" |> Encode.string )
          , ( "count", 4 |> Encode.int )
          ]
        ] |> Encode.list Encode.object
      }
    --}
    )
  , ( ( "GET", "layout/options" )
    {--, Real --}
    {--}, Mock 1000
      { header =
        [ ( "etag", "OPTIONS-ETAG" )
        ] |> Dict.fromList
      , body =
        [ ( "role",    [ "admin", "upload" ]         |> Encode.list Encode.string )
        , ( "gender",  [ "male", "female", "other" ] |> Encode.list Encode.string )
        , ( "quality", [ "high", "low" ]             |> Encode.list Encode.string )
        ] |> Encode.object
      }
    --}
    )
  , ( ( "POST", "upload" )
    {--}, Real --}
    {--, Mock 3000
      { header =
        [ ( "x-upload-id", "3" )
        ] |> Dict.fromList
      , body = Encode.null
      )
    --}
    )
  , ( ( "GET", "upload/:id" )
    {--, Real --}
    {--}, Mock 1000
      { header =
        [ ( "etag", "UPLOAD-ETAG" )
        ] |> Dict.fromList
      , body =
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
        , ( "state"
          , [ ( "state", "working" |> Encode.string )
            ] |> Encode.object
          )
        ] |> Encode.object
      }
    --}
    )
  , ( ( "PUT", "upload/:id/info" )
    {--, Real --}
    {--}, Mock 1000
      { header = Dict.empty
      , body = Encode.null
      }
    --}
    )
  , ( ( "PUT", "upload/:id/detail" )
    {--, Real --}
    {--}, Mock 1000
      { header = Dict.empty
      , body = Encode.null
      }
    --}
    )
  , ( ( "PUT", "upload/:id/complete" )
    {--, Real --}
    {--}, Mock 1000
      { header = Dict.empty
      , body = Encode.null
      }
    --}
    )
  , ( ( "PUT", "upload/:id/work" )
    {--, Real --}
    {--}, Mock 1000
      { header = Dict.empty
      , body = Encode.null
      }
    --}
    )
  , ( ( "DELETE", "upload/:id" )
    {--, Real --}
    {--}, Mock 1000
      { header = Dict.empty
      , body = Encode.null
      }
    --}
    )
  , ( ( "GET", "uploads" )
    {--, Real --}
    {--}, Mock 300
      { header =
        [ ( "x-paging-max", "10" )
        ] |> Dict.fromList
      , body =
        [ [ ( "id",     1        |> Encode.int )
          , ( "name",   "text-1" |> Encode.string )
          , ( "gender", "male"   |> Encode.string )
          , ( "roles", ["admin"] |> Encode.list Encode.string )
          , ( "comments", []     |> Encode.list Encode.object )
          ]
        , [ ( "id",     2        |> Encode.int )
          , ( "name",   "text-2" |> Encode.string )
          , ( "gender", "female" |> Encode.string )
          , ( "roles", [] |> Encode.list Encode.string )
          , ( "comments"
            , [ [ ( "user", "master" |> Encode.string )
                , ( "text", "LGTM" |> Encode.string )
                , ( "likes", [] |> Encode.list Encode.object )
                ]
              , [ ( "user", "guest" |> Encode.string )
                , ( "text", "looks goot to me" |> Encode.string )
                , ( "likes"
                  , [ [ ( "user", "master" |> Encode.string )
                      , ( "text", "looks great to me!" |> Encode.string )
                      ]
                    , [ ( "user", "guest" |> Encode.string )
                      , ( "text", "i agree" |> Encode.string )
                      ]
                    ] |> Encode.list Encode.object
                  )
                ]
              ] |> Encode.list Encode.object
            )
          ]
        , [ ( "id",     3        |> Encode.int )
          , ( "name",   "text-3" |> Encode.string )
          , ( "gender", "other"  |> Encode.string )
          , ( "roles", ["admin","upload"] |> Encode.list Encode.string )
          , ( "comments"
            , [ [ ( "user", "master" |> Encode.string )
                , ( "text", "this is great!" |> Encode.string )
                , ( "likes", [] |> Encode.list Encode.object )
                ]
              ] |> Encode.list Encode.object
            )
          ]
        ] |> Encode.list Encode.object
      }
    --}
    )
  , ( ( "GET", "uploads/:id" )
    {--, Real --}
    {--}, Mock 1000
      { header =
        [ ( "etag", "UPLOAD-ETAG" )
        ] |> Dict.fromList
      , body =
        [ ( "id", 1 |> Encode.int )
        , ( "name", "John Doe" |> Encode.string )
        , ( "gender",   "male" |> Encode.string )
        ] |> Encode.object
      }
    --}
    )
  , ( ( "PUT", "uploads/:id/info" )
    {--, Real --}
    {--}, Mock 1000
      { header = Dict.empty
      , body = Encode.null
      }
    --}
    )
  , ( ( "PUT", "uploads/:id/detail" )
    {--, Real --}
    {--}, Mock 1000
      { header = Dict.empty
      , body = Encode.null
      }
    --}
    )
  ]
