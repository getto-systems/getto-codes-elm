module GettoUpload.Layout.Menu exposing
  ( Menu
  , Item
  , item
  , icon
  , href
  , children
  )
import GettoUpload.Layout.Fa as Fa

type alias Menu = List ( String, List Item )

type Item = Item Info (List Item)

type alias Info =
  { icon : Fa.Icon
  , href : String
  }

item : Fa.Icon -> String -> List Item -> Item
item iconData hrefData items =
  Item
    { icon = iconData
    , href = hrefData
    }
    items

icon : Item -> Fa.Icon
icon = info >> .icon

href : Item -> String
href = info >> .href

info : Item -> Info
info (Item data _) = data

children : Item -> List Item
children (Item _ items) = items
