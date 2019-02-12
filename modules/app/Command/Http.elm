module GettoUpload.Command.Http exposing
  ( Model
  , Payload
  , Header
  , init
  , state
  , response
  , responseHeader
  , responseBody
  , stateTo
  , clear
  , request
  , track
  , payload
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

type Model header body = Model
  { state    : HttpView.State header body
  , response : Maybe (HttpView.Response header body)
  }

type Payload model header body = Payload String (model -> Request header body)
type Request header body
  = Get    (RequestInner header body QueryEncode.Value)
  | Post   (RequestInner header body Encode.Value)
  | Put    (RequestInner header body Encode.Value)
  | Delete (RequestInner header body Encode.Value)
  | Upload (RequestInner header body Part.Value)

type alias RequestInner header body params =
  { url      : String
  , headers  : List Header
  , params   : params
  , response :
    { header : HeaderDecode.Decoder header
    , body   : Decode.Decoder body
    }
  , timeout  : Float
  }

type alias Header = ( String, String )

init : Model header body
init = Model
  { state    = HttpView.Ready Nothing
  , response = Nothing
  }

state : Model header body -> HttpView.State header body
state (Model model) = model.state

response : Model header body -> Maybe (HttpView.Response header body)
response (Model model) = model.response

responseHeader : Model header body -> Maybe header
responseHeader = response >> Maybe.map HttpView.header

responseBody : Model header body -> Maybe body
responseBody = response >> Maybe.map HttpView.body

stateTo : HttpView.State header body -> Model header body -> Model header body
stateTo value (Model model) =
  case value of
    HttpView.Ready (Just (Ok data)) -> Model { model | state = value, response = Just data }
    _ -> Model { model | state = value }

clear : Model header body -> Model header body
clear (Model model) = Model { model | state = HttpView.Ready Nothing, response = Nothing }

request : String -> Payload model header body -> (HttpView.State header body -> msg) -> model -> Cmd msg
request signature (Payload marker req) msg model =
  let
    headers = .headers >> List.map (\(key,value) -> Http.header key value)

    query   = .params >> QueryEncode.encode
    json    = .params >> Http.jsonBody
    part    = .params >> Part.toBody

    trackMarker = Just ( marker |> tracker signature )
  in
    [ HttpView.Connecting Nothing |> Task.succeed |> Task.perform msg
    , Mock.request <| -- Real.request <|
      case model |> req of
        Get data ->
          { method   = "GET"
          , headers  = data |> headers
          , url      = data.url ++ (data |> query)
          , body     = Http.emptyBody
          , response = data.response
          , timeout  = Just data.timeout
          , tracker  = trackMarker
          , msg      = msg
          }
        Post data ->
          { method   = "POST"
          , headers  = data |> headers
          , url      = data.url
          , body     = data |> json
          , response = data.response
          , timeout  = Just data.timeout
          , tracker  = trackMarker
          , msg      = msg
          }
        Put data ->
          { method   = "PUT"
          , headers  = data |> headers
          , url      = data.url
          , body     = data |> json
          , response = data.response
          , timeout  = Just data.timeout
          , tracker  = trackMarker
          , msg      = msg
          }
        Delete data ->
          { method   = "DELETE"
          , headers  = data |> headers
          , url      = data.url
          , body     = data |> json
          , response = data.response
          , timeout  = Just data.timeout
          , tracker  = trackMarker
          , msg      = msg
          }
        Upload data ->
          { method   = "POST"
          , headers  = data |> headers
          , url      = data.url
          , body     = data |> part
          , response = data.response
          , timeout  = Just data.timeout
          , tracker  = trackMarker
          , msg      = msg
          }
    ] |> Cmd.batch

track : String -> Payload model header body -> (HttpView.State header body -> msg) -> Sub msg
track signature (Payload marker _) msg =
  let
    toProgress progress =
      case progress of
        Http.Sending   data -> { current = data.sent, size = data.size } |> HttpView.Sending
        Http.Receiving data ->
          data.size
          |> Maybe.map (\size -> { current = data.received, size = size })
          |> HttpView.Receiving
  in
    (toProgress >> Just >> HttpView.Connecting >> msg) |> Http.track (marker |> tracker signature)

tracker : String -> String -> String
tracker signature marker = signature ++ ":" ++ marker

payload : String -> (model -> Request header body) -> Payload model header body
payload = Payload

get : RequestInner header body QueryEncode.Value -> Request header body
get = Get

post : RequestInner header body Encode.Value -> Request header body
post = Post

put : RequestInner header body Encode.Value -> Request header body
put = Put

delete : RequestInner header body Encode.Value -> Request header body
delete = Delete

upload : RequestInner header body Part.Value -> Request header body
upload = Upload
