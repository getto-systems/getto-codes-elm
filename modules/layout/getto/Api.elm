module GettoUpload.Layout.Api exposing
  ( upload
  )
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Frame.Credential as Credential
import GettoUpload.Extension.Command.Auth as Auth
import GettoUpload.Extension.Command.Http as Http
import GettoUpload.Env.App as Env

type alias Info layout app appMsg =
  { url : String
  , headers : Frame.Model layout app appMsg -> List Http.Header
  }

upload : (( String, (Frame.Model layout app appMsg -> List Http.Header)) -> Http.Request (Frame.Model layout app appMsg) data) -> String -> Http.Request (Frame.Model layout app appMsg) data
upload request pathTo =
  ( Env.api.upload ++ pathTo
  , Frame.auth >> Auth.credential >> Credential.token >>
      \token ->
        [ ( "Authorization", token )
        ]
  ) |> request
