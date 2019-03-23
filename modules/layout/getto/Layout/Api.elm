module GettoCodes.Layout.Api exposing
  ( headers
  , url
  )
import GettoCodes.Layout.Frame as Frame
import GettoCodes.Layout.Frame.Credential as Credential
import GettoCodes.Command.Auth as Auth
import GettoCodes.Command.Http as Http
import GettoCodes.Env as Env

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
