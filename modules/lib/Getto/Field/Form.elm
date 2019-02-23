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
  )
import Getto.Field as Field

import Set exposing ( Set )

type alias Model form a =
  { field : Field.Model a
  , prop  : Prop form a
  }

type alias Between form a =
  { gteq : Model form a
  , lteq : Model form a
  }

type Prop form a = Prop (Getter form a) (Setter form a)
type alias Getter form a = form -> Field.Model a
type alias Setter form a = Field.Model a -> form -> form

prop : Getter form a -> Setter form a -> Prop form a
prop = Prop

init : Prop form a -> Field.Model a -> Model form a
init formProp formField =
  { field = formField
  , prop  = formProp
  }

at : Prop form a -> form -> Field.Model a
at (Prop getter _) = getter

setIf : Prop form a -> Maybe a -> form -> form
setIf (Prop getter setter) value form =
  case value of
    Nothing  -> form
    Just val -> form |> setter (form |> getter |> Field.set val)

set : Prop form a -> a -> form -> form
set (Prop getter setter) value form =
  form |> setter (form |> getter |> Field.set value)

toggle : Prop form (Set comparable) -> comparable -> form -> form
toggle (Prop getter setter) value form =
  form |> setter (form |> getter |> Field.toggle value)
