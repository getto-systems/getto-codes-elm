module GettoUpload.Layout.Api exposing
  ( request
  )
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Frame.Credential as Credential
import GettoUpload.Command.Auth as Auth
import GettoUpload.Command.Http as Http
import GettoUpload.Env.App as Env

type alias Info layout app appMsg =
  { url : String
  , headers : Frame.Model layout app appMsg -> List Http.Header
  }

request : (( String, (Frame.Model layout app appMsg -> List Http.Header)) -> Http.Request (Frame.Model layout app appMsg) header body) -> Http.Request (Frame.Model layout app appMsg) header body
request req =
  ( Env.api.host
  , Frame.auth >> Auth.credential >> Credential.token >>
      \token ->
        [ ( "Authorization", token )
        ]
  ) |> req
