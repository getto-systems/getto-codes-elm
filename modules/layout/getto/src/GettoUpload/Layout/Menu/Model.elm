module GettoUpload.Layout.Menu.Model exposing
  ( Menu
  , Item(..)
  , item
  )
import GettoUpload.Layout.Fa as Fa

type alias Menu = List ( String, List Item )

type Item
  = Item Fa.Icon String (List Item)

item : Fa.Icon -> String -> List Item -> Item
item = Item
