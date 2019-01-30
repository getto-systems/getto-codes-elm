module GettoUpload.Layout.Command.Http exposing
  ( Entry
  , State
  , Request
  , Header
  , empty
  , stateTo
  , clear
  , request
  , track
  , get
  , post
  , put
  , delete
  , upload
  )
import GettoUpload.Layout.Command.Http.Mock as Mock

import Getto.Url.Query.Encode as QueryEncode
import Getto.Http.Part as Part

import Json.Encode as Encode
import Json.Decode as Decode
import Http

type Entry data = Entry
  { state : State data
  , data  : Maybe data
  }

type State data
  = Empty
  | Loading
  | Progress Http.Progress
  | Success data
  | Failure Http.Error

type Request model data
  = Get    (RequestInner model data QueryEncode.Value)
  | Post   (RequestInner model data Encode.Value)
  | Put    (RequestInner model data Encode.Value)
  | Delete (RequestInner model data Encode.Value)
  | Upload (RequestInner model data Part.Value)

type alias RequestInner model data params =
  { url     : String
  , headers : model -> List Header
  , params  : model -> params
  , decoder : Decode.Decoder data
  , timeout : Float
  , tracker : Maybe String
  }

type alias Header = ( String, String )

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

empty : Entry model
empty = Entry
  { state = Empty
  , data  = Nothing
  }

stateTo : State model -> Entry model -> Entry model
stateTo state (Entry entry) =
  case state of
    Success data -> Entry { entry | state = state, data = Just data }
    _            -> Entry { entry | state = state }

clear : Entry model -> Entry model
clear (Entry entry) = Entry { entry | data = Nothing }

request : Request model data -> (State data -> msg) -> model -> Cmd msg
request req msg model =
  let
    headers data = model |> data.headers |> List.map (\(key,value) -> Http.header key value)
    query   data = model |> data.params |> QueryEncode.encode
    json    data = model |> data.params |> Http.jsonBody
    part    data = model |> data.params |> Part.toBody
    requestMsg = toState >> msg
  in
    Mock.request <| -- httpRequest <|
      case req of
        Get data ->
          { method  = "GET"
          , headers = data |> headers
          , url     = data.url ++ (data |> query)
          , body    = Http.emptyBody
          , decoder = data.decoder
          , timeout = Just data.timeout
          , tracker = data.tracker
          , msg     = requestMsg
          }
        Post data ->
          { method  = "POST"
          , headers = data |> headers
          , url     = data.url
          , body    = data |> json
          , decoder = data.decoder
          , timeout = Just data.timeout
          , tracker = data.tracker
          , msg     = requestMsg
          }
        Put data ->
          { method  = "PUT"
          , headers = data |> headers
          , url     = data.url
          , body    = data |> json
          , decoder = data.decoder
          , timeout = Just data.timeout
          , tracker = data.tracker
          , msg     = requestMsg
          }
        Delete data ->
          { method  = "DELETE"
          , headers = data |> headers
          , url     = data.url
          , body    = data |> json
          , decoder = data.decoder
          , timeout = Just data.timeout
          , tracker = data.tracker
          , msg     = requestMsg
          }
        Upload data ->
          { method  = "POST"
          , headers = data |> headers
          , url     = data.url
          , body    = data |> part
          , decoder = data.decoder
          , timeout = Just data.timeout
          , tracker = data.tracker
          , msg     = requestMsg
          }

toState : Result Http.Error data -> State data
toState result =
  case result of
    Ok  data  -> Success data
    Err error -> Failure error

httpRequest : RequestData data msg -> Cmd msg
httpRequest data =
  Http.request
    { method  = data.method
    , headers = data.headers
    , url     = data.url
    , body    = data.body
    , expect  = data.decoder |> Http.expectJson data.msg
    , timeout = data.timeout
    , tracker = data.tracker
    }

track : Request model data -> (State data -> msg) -> Sub msg
track req msg =
  let
    trackerData =
      case req of
        Get    data -> data.tracker
        Post   data -> data.tracker
        Put    data -> data.tracker
        Delete data -> data.tracker
        Upload data -> data.tracker
  in
    case trackerData of
      Nothing -> Sub.none
      Just tracker -> (Progress >> msg) |> Http.track tracker

get : RequestInner model data QueryEncode.Value -> Request model data
get = Get

post : RequestInner model data Encode.Value -> Request model data
post = Post

put : RequestInner model data Encode.Value -> Request model data
put = Put

delete : RequestInner model data Encode.Value -> Request model data
delete = Delete

upload : RequestInner model data Part.Value -> Request model data
upload = Upload
