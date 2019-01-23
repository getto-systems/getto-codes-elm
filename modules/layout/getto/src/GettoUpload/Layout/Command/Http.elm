module GettoUpload.Layout.Command.Http exposing
  ( State(..)
  , Init
  , Model
  , Entry
  , Response
  , init
  , entry
  , get
  , post
  , put
  , delete
  , upload
  , requestLayout
  , requestApp
  , execLayout
  , execApp
  , layoutResponse
  , appResponse
  , layoutState
  , appState
  )
import GettoUpload.Env.App as Env

import Getto.Prop as Prop exposing ( Prop )
import Getto.Url.Query.Encode as QueryEncode

import Json.Encode as Encode
import Json.Decode as Decode
import File exposing ( File )
import Http

timeout =
  { get    = 10 * 1000
  , post   = 60 * 1000
  , put    = 60 * 1000
  , delete = 60 * 1000
  , upload = 30 * 60 * 1000
  }

type Model layout app = Model (Struct layout app)

type alias Struct layout app =
  { layout : StructInfo layout
  , app    : StructInfo app
  }

type alias StructInfo struct =
  { struct : struct
  , props  : Props struct
  }

type alias Props struct = List (Prop struct Entry)

type Entry = Entry Info

type alias Info =
  { state : State Decode.Value
  , cmd   : Request
  }

type State response
  = Empty
  | Connecting
  | Done response
  | Error Http.Error

type Request
  = None
  | Get    RequestStruct
  | Post   RequestStruct
  | Put    RequestStruct
  | Delete RequestStruct
  | Upload RequestStruct

type alias RequestStruct =
  { headers : Headers
  , url     : String
  , body    : Http.Body
  }

type alias Headers = List Http.Header

type alias Response = Result Http.Error Decode.Value

type alias Init struct = ( struct, Props struct )

init : (Init layout,Init app) -> () -> Model layout app
init ((layout,layoutProps),(app,appProps)) _ =
  Model
    { layout =
      { struct = layout
      , props  = layoutProps
      }
    , app =
      { struct = app
      , props  = appProps
      }
    }

entry : Entry
entry =
  Entry
    { state = Empty
    , cmd   = None
    }

type alias RawHeaders = List ( String, String )

get : RawHeaders -> String -> QueryEncode.Value -> Request
get headers path query =
  ( headers
  , path ++ (query |> QueryEncode.encode)
  , Http.emptyBody
  )
  |> request Get

post : RawHeaders -> String -> Encode.Value -> Request
post headers path data =
  ( headers
  , path
  , data |> Http.jsonBody
  )
  |> request Post

put : RawHeaders -> String -> Encode.Value -> Request
put headers path data =
  ( headers
  , path
  , data |> Http.jsonBody
  )
  |> request Put

delete : RawHeaders -> String -> Encode.Value -> Request
delete headers path data =
  ( headers
  , path
  , data |> Http.jsonBody
  )
  |> request Delete

upload : RawHeaders -> String -> File -> Request
upload headers path data =
  ( headers
  , path
  , data |> Http.fileBody
  )
  |> request Upload

request : (RequestStruct -> Request) -> ( RawHeaders, String, Http.Body ) -> Request
request req (headers,path,body) =
  req
    { headers = headers |> List.map (\(key,value) -> Http.header key value)
    , url     = Env.apiRoot ++ path
    , body    = body
    }

requestLayout : Prop layout Entry -> Request -> Model layout app -> Model layout app
requestLayout prop req (Model m) =
  let
    layout = m.layout
  in
    Model { m | layout = { layout | struct = layout.struct |> Prop.update prop (setCmd req) } }

requestApp : Prop app Entry -> Request -> Model layout app -> Model layout app
requestApp prop req (Model m) =
  let
    app = m.app
  in
    Model { m | app = { app | struct = app.struct |> Prop.update prop (setCmd req) } }

setCmd : Request -> Entry -> Entry
setCmd req (Entry m) = Entry { m | cmd = req }

execLayout : (Prop layout Entry -> Response -> msg) -> Model layout app -> ( Model layout app, Cmd msg )
execLayout msg (Model m) =
  m.layout
  |> execProps msg
  |>
    (\(struct,cmds) ->
      ( Model { m | layout = struct }
      , Cmd.batch cmds
      )
    )

execApp : (Prop app Entry -> Response -> msg) -> Model layout app -> ( Model layout app, Cmd msg )
execApp msg (Model m) =
  m.app
  |> execProps msg
  |>
    (\(struct,cmds) ->
      ( Model { m | app = struct }
      , Cmd.batch cmds
      )
    )

execProps : (Prop struct Entry -> Response -> msg) -> StructInfo struct -> ( StructInfo struct, List (Cmd msg) )
execProps msg struct =
  struct.props
  |> List.foldl
    (\prop (acc,cmds) ->
      let
        (newEntry,cmd) = acc.struct |> Prop.get prop |> exec (msg prop)
      in
        ( { acc | struct = acc.struct |> Prop.set prop newEntry }
        , cmd :: cmds
        )
    )
    (struct,[])

exec : (Response -> msg) -> Entry -> ( Entry, Cmd msg )
exec msg (Entry m) =
  case m.cmd of
    None  -> ( Entry m, Cmd.none )
    Get struct ->
      ( Entry (m |> execState)
      , Http.request
        { method  = "GET"
        , headers = struct.headers
        , url     = struct.url
        , body    = struct.body
        , expect  = Decode.value |> Http.expectJson msg
        , timeout = Just timeout.get
        , tracker = Nothing
        }
      )
    Post struct ->
      ( Entry (m |> execState)
      , Http.request
        { method  = "POST"
        , headers = struct.headers
        , url     = struct.url
        , body    = struct.body
        , expect  = Decode.value |> Http.expectJson msg
        , timeout = Just timeout.post
        , tracker = Nothing
        }
      )
    Put struct ->
      ( Entry (m |> execState)
      , Http.request
        { method  = "PUT"
        , headers = struct.headers
        , url     = struct.url
        , body    = struct.body
        , expect  = Decode.value |> Http.expectJson msg
        , timeout = Just timeout.put
        , tracker = Nothing
        }
      )
    Delete struct ->
      ( Entry (m |> execState)
      , Http.request
        { method  = "DELETE"
        , headers = struct.headers
        , url     = struct.url
        , body    = struct.body
        , expect  = Decode.value |> Http.expectJson msg
        , timeout = Just timeout.delete
        , tracker = Nothing
        }
      )
    Upload struct ->
      ( Entry (m |> execState)
      , Http.request
        { method  = "POST"
        , headers = struct.headers
        , url     = struct.url
        , body    = struct.body
        , expect  = Decode.value |> Http.expectJson msg
        , timeout = Just timeout.upload
        , tracker = Nothing
        }
      )

execState : Info -> Info
execState m = { m | state = Connecting, cmd = None }

layoutResponse : Prop layout Entry -> Response -> Model layout app -> Model layout app
layoutResponse prop result (Model m) =
  let
    layout = m.layout
  in
    Model { m | layout = { layout | struct = layout.struct |> Prop.update prop (response result) } }

appResponse : Prop app Entry -> Response -> Model layout app -> Model layout app
appResponse prop result (Model m) =
  let
    app = m.app
  in
    Model { m | app = { app | struct = app.struct |> Prop.update prop (response result) } }

response : Response -> Entry -> Entry
response result (Entry m) =
  case result of
    Ok  res   -> Entry { m | state = Done res }
    Err error -> Entry { m | state = Error error }

layoutState : Prop layout Entry -> Decode.Decoder response -> Model layout app -> State response
layoutState prop decoder (Model m) = m.layout.struct |> Prop.get prop |> state decoder

appState : Prop app Entry -> Decode.Decoder response -> Model layout app -> State response
appState prop decoder (Model m) = m.app.struct |> Prop.get prop |> state decoder

state : Decode.Decoder response -> Entry -> State response
state decoder (Entry m) =
  case m.state of
    Empty       -> Empty
    Connecting  -> Connecting
    Error error -> Error error
    Done  res   ->
      case res |> Decode.decodeValue decoder of
        Ok  value -> Done value
        Err error -> error |> Decode.errorToString |> Http.BadBody |> Error
