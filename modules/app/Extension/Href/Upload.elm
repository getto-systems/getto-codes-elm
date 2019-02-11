module GettoUpload.Extension.Href.Upload exposing
  ( list
  , edit
  )
import GettoUpload.Extension.Href as Href

import Getto.Url.Query.Encode as QueryEncode

href params path = "upload/" ++ path |> Href.internal params

list    = "list.html" |> href QueryEncode.empty
edit id = "edit.html" |> href ( [ ( "id", id |> QueryEncode.int ) ] |> QueryEncode.object )
