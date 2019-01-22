module GettoUpload.Layout.Href.Home exposing
  ( menu_
  , index
  )
import GettoUpload.Layout.Href as Href
import GettoUpload.Layout.Menu as Menu
import GettoUpload.Layout.Fa as Fa

menu_ : List Menu.Item
menu_ =
  [ Menu.item (Fa.solid "home") index []
  ]

index = "index.html" |> Href.prependRoot
