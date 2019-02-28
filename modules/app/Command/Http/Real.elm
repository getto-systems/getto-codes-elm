module GettoUpload.Command.Http.Real exposing
  ( request
  )
import GettoUpload.View.Http as HttpView

import Getto.Http.Header.Decode as HeaderDecode

import Json.Decode as Decode
import Http

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

request : RequestData response msg -> Cmd msg
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


expectJson : (HttpView.Migration response -> msg) -> HttpView.ResponseDecoder response -> Http.Expect msg
expectJson msg decoder =
  Http.expectStringResponse (toMigration >> msg) <|
    \response ->
      case response of
        Http.BadUrl_ url   -> HttpView.BadUrl url   |> Err
        Http.Timeout_      -> HttpView.Timeout      |> Err
        Http.NetworkError_ -> HttpView.NetworkError |> Err

        Http.BadStatus_ metadata _ ->
          case metadata.statusCode of
            304 -> Nothing |> Ok

            400 -> HttpView.BadRequest           |> Err
            401 -> HttpView.Unauthorized         |> Err
            403 -> HttpView.Forbidden            |> Err
            404 -> HttpView.NotFound             |> Err
            409 -> HttpView.Conflict             |> Err
            422 -> HttpView.UnprocessableEntity  |> Err
            428 -> HttpView.PreconditionRequired |> Err

            status -> HttpView.BadStatus status |> Err

        Http.GoodStatus_ metadata raw ->
          case raw |> Decode.decodeString Decode.value of
            Err jsonError ->
              jsonError
              |> Decode.errorToString
              |> Debug.log "json"
              |> HttpView.BadResponse
              |> Err

            Ok body ->
              case { header = metadata.headers, body = body } |> decoder of
                Err error -> error |> HttpView.BadResponse |> Err
                Ok  res   -> res   |> Just |> Ok

toMigration : Result HttpView.Error (Maybe response) -> HttpView.Migration response
toMigration result =
  case result of
    Err err        -> err |> HttpView.failure
    Ok  (Just res) -> res |> HttpView.success
    Ok  Nothing    -> HttpView.notModified
