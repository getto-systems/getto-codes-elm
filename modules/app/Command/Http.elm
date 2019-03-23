module GettoCodes.Command.Http exposing
  ( Tracker
  , Header
  , request
  , track
  , tracker
  , get
  , getIfNoneMatch
  , put
  , putIfMatch
  , delete
  , post
  , upload
  )
import GettoCodes.Command.Http.Real as Real
import GettoCodes.Command.Http.Mock as Mock
import GettoCodes.View.Http as HttpView

import Getto.Url.Query.Encode as QueryEncode
import Getto.Http.Part as Part

import Json.Encode as Encode
import Http
import Task

type Tracker model response = Tracker String (model -> Request response)
type Request response
  = Get    (Maybe String) (RequestInner response QueryEncode.Value)
  | Put    (Maybe String) (RequestInner response Encode.Value)
  | Delete (Maybe String) (RequestInner response Encode.Value)
  | Post   (RequestInner response Encode.Value)
  | Upload (RequestInner response Part.Value)

type alias RequestInner response params =
  { url      : String
  , headers  : List Header
  , params   : params
  , response : HttpView.ResponseDecoder response
  , timeout  : Float
  }

type alias Header = ( String, String )

request : String -> Tracker model response -> (HttpView.Migration response -> msg) -> model -> Cmd msg
request signature (Tracker marker req) msg model =
  let
    headers = .headers >> List.map (\(key,value) -> Http.header key value)

    query   = .params >> QueryEncode.encode
    json    = .params >> Http.jsonBody
    part    = .params >> Part.toBody

    trackMarker = Just ( marker |> toTrackMarker signature )
  in
    [ HttpView.load |> Task.succeed |> Task.perform msg
    , Mock.request <| -- Real.request <|
      case model |> req of
        Get etag data ->
          { method   = "GET"
          , headers  = data |> headers |> appendIfNoneMatch etag
          , url      = data.url ++ (data |> query)
          , body     = Http.emptyBody
          , response = data.response
          , timeout  = Just data.timeout
          , tracker  = trackMarker
          , msg      = msg
          }
        Put etag data ->
          { method   = "PUT"
          , headers  = data |> headers |> appendIfNoneMatch etag
          , url      = data.url
          , body     = data |> json
          , response = data.response
          , timeout  = Just data.timeout
          , tracker  = trackMarker
          , msg      = msg
          }
        Delete etag data ->
          { method   = "DELETE"
          , headers  = data |> headers |> appendIfMatch etag
          , url      = data.url
          , body     = data |> json
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

appendIfNoneMatch : Maybe String -> List Http.Header -> List Http.Header
appendIfNoneMatch = appendEtag "If-None-Match"

appendIfMatch : Maybe String -> List Http.Header -> List Http.Header
appendIfMatch = appendEtag "If-Match"

appendEtag : String -> Maybe String -> List Http.Header -> List Http.Header
appendEtag key etag headers =
  case etag of
    Nothing  -> headers
    Just tag -> headers ++ [ Http.header key tag ]

track : String -> Tracker model response -> (HttpView.Migration (HttpView.Response header body) -> msg) -> Sub msg
track signature (Tracker marker _) msg =
  let
    toProgress progress =
      case progress of
        Http.Sending   data ->
          if data.sent == data.size
            then HttpView.Proccessing
            else { current = data.sent, size = data.size } |> HttpView.Sending
        Http.Receiving data ->
          data.size
          |> Maybe.map (\size -> { current = data.received, size = size })
          |> HttpView.Receiving
  in
    (toProgress >> HttpView.transfer >> msg) |> Http.track (marker |> toTrackMarker signature)

toTrackMarker : String -> String -> String
toTrackMarker signature marker = signature ++ ":" ++ marker

tracker : String -> (model -> Request response) -> Tracker model response
tracker = Tracker

get : RequestInner response QueryEncode.Value -> Request response
get = Get Nothing

getIfNoneMatch : Maybe String -> RequestInner response QueryEncode.Value -> Request response
getIfNoneMatch = Get

put : RequestInner response Encode.Value -> Request response
put = Put Nothing

putIfMatch : Maybe String -> RequestInner response Encode.Value -> Request response
putIfMatch = Put

delete : Maybe String -> RequestInner response Encode.Value -> Request response
delete = Delete

post : RequestInner response Encode.Value -> Request response
post = Post

upload : RequestInner response Part.Value -> Request response
upload = Upload
