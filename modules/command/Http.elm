module GettoUpload.Command.Http exposing
  ( Entry
  , Request
  , Header
  , empty
  , state
  , response
  , responseHeader
  , responseBody
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
--import GettoUpload.Command.Http.Real as Real
import GettoUpload.Command.Http.Mock as Mock
import GettoUpload.View.Http as HttpView

import Getto.Url.Query.Encode as QueryEncode
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Http.Part as Part

import Json.Encode as Encode
import Json.Decode as Decode
import Http
import Task

type Entry header body = Entry
  { state    : HttpView.State header body
  , response : Maybe (HttpView.Response header body)
  }

type Request model header body
  = Get    (RequestInner model header body QueryEncode.Value)
  | Post   (RequestInner model header body Encode.Value)
  | Put    (RequestInner model header body Encode.Value)
  | Delete (RequestInner model header body Encode.Value)
  | Upload (RequestInner model header body Part.Value)

type alias RequestInner model header body params =
  { url      : String
  , headers  : model -> List Header
  , params   : model -> params
  , response :
    { header : HeaderDecode.Decoder header
    , body   : Decode.Decoder body
    }
  , timeout  : Float
  }

type alias Header = ( String, String )

empty : Entry header body
empty = Entry
  { state    = HttpView.Empty
  , response = Nothing
  }

state : Entry header body -> HttpView.State header body
state (Entry entry) = entry.state

response : Entry header body -> Maybe (HttpView.Response header body)
response (Entry entry) = entry.response

responseHeader : Entry header body -> Maybe header
responseHeader = response >> Maybe.map HttpView.header

responseBody : Entry header body -> Maybe body
responseBody = response >> Maybe.map HttpView.body

stateTo : HttpView.State header body -> Entry header body -> Entry header body
stateTo value (Entry entry) =
  case value of
    HttpView.Success data -> Entry { entry | state = value, response = Just data }
    _                     -> Entry { entry | state = value }

clear : Entry header body -> Entry header body
clear (Entry entry) = Entry { entry | state = HttpView.Empty, response = Nothing }

request : Request model header body -> (HttpView.State header body -> msg) -> model -> Cmd msg
request req msg model =
  let
    headers data = model |> data.headers |> List.map (\(key,value) -> Http.header key value)
    query   data = model |> data.params |> QueryEncode.encode
    json    data = model |> data.params |> Http.jsonBody
    part    data = model |> data.params |> Part.toBody

    marker = req |> trackMarker |> Just
  in
    [ HttpView.Loading |> Task.succeed |> Task.perform msg
    , Mock.request <| -- Real.request <|
      case req of
        Get data ->
          { method   = "GET"
          , headers  = data |> headers
          , url      = data.url ++ (data |> query)
          , body     = Http.emptyBody
          , response = data.response
          , timeout  = Just data.timeout
          , tracker  = marker
          , msg      = msg
          }
        Post data ->
          { method   = "POST"
          , headers  = data |> headers
          , url      = data.url
          , body     = data |> json
          , response = data.response
          , timeout  = Just data.timeout
          , tracker  = marker
          , msg      = msg
          }
        Put data ->
          { method   = "PUT"
          , headers  = data |> headers
          , url      = data.url
          , body     = data |> json
          , response = data.response
          , timeout  = Just data.timeout
          , tracker  = marker
          , msg      = msg
          }
        Delete data ->
          { method   = "DELETE"
          , headers  = data |> headers
          , url      = data.url
          , body     = data |> json
          , response = data.response
          , timeout  = Just data.timeout
          , tracker  = marker
          , msg      = msg
          }
        Upload data ->
          { method   = "POST"
          , headers  = data |> headers
          , url      = data.url
          , body     = data |> part
          , response = data.response
          , timeout  = Just data.timeout
          , tracker  = marker
          , msg      = msg
          }
    ] |> Cmd.batch

trackMarker : Request model header body -> String
trackMarker req =
  let
    appendUrl data marker = marker ++ data.url
  in
    case req of
      Get    data -> "GET:"    |> appendUrl data
      Post   data -> "POST:"   |> appendUrl data
      Put    data -> "PUT:"    |> appendUrl data
      Delete data -> "DELETE:" |> appendUrl data
      Upload data -> "UPLOAD:" |> appendUrl data

track : Request model header body -> (HttpView.State header body -> msg) -> Sub msg
track req msg =
  let
    marker = req |> trackMarker

    toProgress progress =
      case progress of
        Http.Sending   data -> HttpView.Sending data
        Http.Receiving data -> HttpView.Receiving data
  in
    (toProgress >> HttpView.Progress >> msg) |> Http.track marker

get : RequestInner model header body QueryEncode.Value -> Request model header body
get = Get

post : RequestInner model header body Encode.Value -> Request model header body
post = Post

put : RequestInner model header body Encode.Value -> Request model header body
put = Put

delete : RequestInner model header body Encode.Value -> Request model header body
delete = Delete

upload : RequestInner model header body Part.Value -> Request model header body
upload = Upload
