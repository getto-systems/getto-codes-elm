module GettoCodes.App.Data.Upload.Href exposing
  ( list
  , list_edit
  , new
  , edit_
  , edit
  )
import GettoCodes.View.Href as Href

import Getto.Url.Query.Encode as QueryEncode

href params path = "data/upload/" ++ path |> Href.internal params

list      = "list.html" |> href QueryEncode.null
list_edit = "list_edit.html" |> href QueryEncode.null
new       = "new.html"  |> href QueryEncode.null
edit_     = "edit.html" |> href QueryEncode.null
edit id   = "edit.html" |> href ( [ ( "id", id |> QueryEncode.int ) ] |> QueryEncode.object )
