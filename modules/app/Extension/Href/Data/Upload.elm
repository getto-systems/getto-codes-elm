module GettoCodes.Extension.Href.Data.Upload exposing
  ( list
  , list_edit
  , new
  , edit_
  , edit
  )
import GettoCodes.Extension.Href as Href

import Getto.Url.Query.Encode as QueryEncode

href params path = "data/upload/" ++ path |> Href.internal params

list      = "list.html" |> href QueryEncode.null
list_edit = "list_edit.html" |> href QueryEncode.null
new       = "new.html"  |> href QueryEncode.null
edit_     = "edit.html" |> href QueryEncode.null
edit id   = "edit.html" |> href ( [ ( "id", id |> QueryEncode.int ) ] |> QueryEncode.object )
