module GettoUpload.Layout.Api exposing
  ( headers
  , url
  )
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Frame.Credential as Credential
import GettoUpload.Command.Auth as Auth
import GettoUpload.Command.Http as Http
import GettoUpload.Env as Env

headers : Frame.Model layout app -> List Http.Header
headers =
  Frame.auth >> Auth.credential >> Credential.token >>
    \token ->
      [ ( "Authorization", token )
      ]

url : List ( String, String ) -> String -> String
url pathInfo path =
  Env.api.host ++
    ( pathInfo
    |> List.foldl
      (\(target,replace) ->
        String.replace (":" ++ target) replace
      )
      path
    )
