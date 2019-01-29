module GettoUpload.Layout.Http.Mock exposing
  ( find
  )
import GettoUpload.Layout.Api as Api

import Getto.Http as Http

import Dict exposing ( Dict )
import Json.Encode as Encode

find : Http.Request -> Maybe Http.Response
find req = dict |> Dict.get (req.method,req.url)

dict : Dict ( String, String ) Http.Response
dict =
  [ ( ( "GET", "state" ), Ok Encode.null )
  ]
  |> List.map (\((method,path),res) -> ( ( method, path |> Api.prependRoot ), res ))
  |> Dict.fromList
