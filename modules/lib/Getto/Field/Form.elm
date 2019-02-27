module Getto.Field.Form exposing
  ( Model
  , Between
  , Prop
  , prop
  , init
  , at
  , setIf
  , set
  , toggle
  , setAttribute
  )
import Getto.Field as Field

import Set exposing ( Set )

type alias Model form attr a =
  { field : Field.Model attr a
  , prop  : Prop form attr a
  }

type alias Between form attr a =
  { gteq : Model form attr a
  , lteq : Model form attr a
  }

type Prop form attr a = Prop (Getter form attr a) (Setter form attr a)
type alias Getter form attr a = form -> Field.Model attr a
type alias Setter form attr a = Field.Model attr a -> form -> form

prop : Getter form attr a -> Setter form attr a -> Prop form attr a
prop = Prop

init : Prop form attr a -> Field.Model attr a -> Model form attr a
init formProp formField =
  { field = formField
  , prop  = formProp
  }

at : Prop form attr a -> form -> Field.Model attr a
at (Prop getter _) = getter

setIf : Prop form attr a -> Maybe a -> form -> form
setIf formProp value =
  case value of
    Nothing  -> identity
    Just val -> set formProp val

set : Prop form attr a -> a -> form -> form
set formProp = Field.set >> update formProp

toggle : Prop form attr (Set comparable) -> comparable -> form -> form
toggle formProp = Field.toggle >> update formProp

setAttribute : Prop form attr value -> attr -> form -> form
setAttribute formProp = Field.setAttribute >> update formProp

update : Prop form attr value -> (Field.Model attr value -> Field.Model attr value) -> form -> form
update (Prop getter setter) f form =
  form |> setter (form |> getter |> f)
