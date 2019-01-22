module GettoUpload.Layout.Menu.Model exposing
  ( Menu
  , Item
  , Info
  , item
  , info
  , children
  )
import GettoUpload.Layout.Fa as Fa

type alias Menu = List ( String, List Item )

type Item
  = Item Info (List Item)

type alias Info =
  { icon : Fa.Icon
  , href : String
  }

item : Fa.Icon -> String -> List Item -> Item
item icon href items = Item { icon = icon, href = href } items

info : Item -> Info
info (Item entry _) = entry

children : Item -> List Item
children (Item _ items) = items
