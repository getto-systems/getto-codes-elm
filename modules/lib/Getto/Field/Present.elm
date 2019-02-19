module Getto.Field.Present exposing
  ( Model
  , Init
  , init
  , between
  , expose
  , string
  , set
  , present
  )
import Getto.Field as Field
import Getto.Field.Form as Form

import Set exposing ( Set )

type Model form = Model
  { name    : String
  , present : Bool
  , form    : form
  }

type alias Init form a = ( Form.Prop form a, Field.Model a -> Bool )
type alias InitBetween form a =
  { gteq : Init form a
  , lteq : Init form a
  }

init : Init form a -> form -> Model (Form.Model form a)
init (prop,f) model =
  let
    field = model |> Form.at prop
  in
    Model
      { name    = field |> Field.name
      , present = field |> f
      , form    = field |> Form.init prop
      }

between : String -> InitBetween form a -> form -> Model (Form.Between form a)
between compositName spec model =
  let
    gteq = model |> init spec.gteq
    lteq = model |> init spec.lteq
  in
    Model
      { name    = compositName
      , present = ( gteq |> isPresent ) || ( lteq |> isPresent )
      , form =
        { gteq = gteq |> form
        , lteq = lteq |> form
        }
      }


isPresent : Model form -> Bool
isPresent (Model model) = model.present

form : Model form -> form
form (Model model) = model.form


expose : Model form -> ( String, Bool, form )
expose (Model model) =
  ( model.name
  , model.present
  , model.form
  )


string : Field.Model String -> Bool
string = present (String.isEmpty >> not)

set : Field.Model (Set String) -> Bool
set = present (Set.isEmpty >> not)

present : (a -> Bool) -> Field.Model a -> Bool
present f = Field.value >> f
