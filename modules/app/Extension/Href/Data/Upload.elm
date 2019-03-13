module GettoUpload.Extension.Href.Data.Upload exposing
  ( list
  , list_edit
  , new
  , edit_
  , edit
  )
import GettoUpload.Extension.Href as Href

import Getto.Url.Query.Encode as QueryEncode

href params path = "data/upload/" ++ path |> Href.internal params

list      = "list.html" |> href QueryEncode.empty
list_edit = "list_edit.html" |> href QueryEncode.empty
new       = "new.html"  |> href QueryEncode.empty
edit_     = "edit.html" |> href QueryEncode.empty
edit id   = "edit.html" |> href ( [ ( "id", id |> QueryEncode.int ) ] |> QueryEncode.object )
