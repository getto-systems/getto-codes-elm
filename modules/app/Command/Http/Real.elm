module GettoUpload.Command.Http.Real exposing
  ( request
  )
import GettoUpload.View.Http as HttpView

import Getto.Http.Header.Decode as HeaderDecode

import Json.Decode as Decode
import Http

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

request : RequestData header body msg -> Cmd msg
request data =
  Http.request
    { method  = data.method
    , headers = data.headers
    , url     = data.url
    , body    = data.body
    , expect  = data.response |> expectJson data.msg
    , timeout = data.timeout
    , tracker = data.tracker
    }


expectJson : (HttpView.Migration header body -> msg) -> Response header body -> Http.Expect msg
expectJson msg decoder =
  Http.expectStringResponse (toMigration >> msg) <|
    \response ->
      case response of
        Http.BadUrl_ url   -> HttpView.BadUrl url   |> Err
        Http.Timeout_      -> HttpView.Timeout      |> Err
        Http.NetworkError_ -> HttpView.NetworkError |> Err

        Http.BadStatus_ metadata _ ->
          case metadata.statusCode of
            400 -> HttpView.BadRequest          |> Err
            401 -> HttpView.Unauthorized        |> Err
            403 -> HttpView.Forbidden           |> Err
            404 -> HttpView.NotFound            |> Err
            422 -> HttpView.UnprocessableEntity |> Err

            status -> HttpView.BadStatus status |> Err

        Http.GoodStatus_ metadata res ->
          case metadata.headers |> HeaderDecode.decode decoder.header of
            Err headerError ->
              headerError
              |> HeaderDecode.errorToString
              |> Debug.log ("bad-header: " ++ (metadata.headers |> Debug.toString))
              |> HttpView.BadHeader
              |> Err

            Ok header ->
              case res |> Decode.decodeString decoder.body of
                Err bodyError ->
                  bodyError
                  |> Decode.errorToString
                  |> Debug.log "bad-body"
                  |> HttpView.BadBody
                  |> Err

                Ok body -> body |> HttpView.response header |> Ok

toMigration : Result HttpView.Error (HttpView.Response header body) -> HttpView.Migration header body
toMigration result =
  case result of
    Ok  res -> res |> HttpView.Success
    Err err -> err |> HttpView.Failure
