module GettoCodes.View.Menu exposing
  ( Menu
  , Item
  , item
  , icon
  , href
  , children
  )
import GettoCodes.View.Href as Href exposing ( Href )
import GettoCodes.View.Icon as IconView exposing ( Icon )

type alias Menu = List ( String, List Item )

type Item = Item Info (List Item)

type alias Info =
  { icon : Icon
  , href : Href
  }

item : Icon -> Href -> List Item -> Item
item iconData hrefData items =
  Item
    { icon = iconData
    , href = hrefData
    }
    items

icon : Item -> Icon
icon = info >> .icon

href : Item -> Href
href = info >> .href

info : Item -> Info
info (Item data _) = data

children : Item -> List Item
children (Item _ items) = items
