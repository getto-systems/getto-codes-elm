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
  , msg      : HttpView.Migration (HttpView.Response header body) -> msg
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
      Just (Mock delay rawHeader rawBody) ->
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

mock : List ( ( String, String ), Request )
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
  , ( ( "GET", "upload/:id" )
    {--, Real --}
    {--}, Mock 1000
      ( [ ( "etag", "UPLOAD-ETAG" )
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
  , ( ( "PUT", "upload/:id/info" )
    {--, Real --}
    {--}, Mock 1000
      ( [ ( "etag", "UPLOAD-ETAG" )
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
    {--}, Mock 300
      ( [ ( "x-paging-max", "10" )
        ] |> Dict.fromList
      )
      ( Encode.list Encode.object
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
        ]
      )
    --}
    )
  ]
