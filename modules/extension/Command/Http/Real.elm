module GettoUpload.Extension.Command.Http.Real exposing
  ( request
  )
import GettoUpload.Extension.View.Http as HttpView

import Json.Decode as Decode
import Http

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

request : RequestData data msg -> Cmd msg
request data =
  Http.request
    { method  = data.method
    , headers = data.headers
    , url     = data.url
    , body    = data.body
    , expect  = data.decoder |> Http.expectJson data.msg
    , timeout = data.timeout
    , tracker = data.tracker
    }
