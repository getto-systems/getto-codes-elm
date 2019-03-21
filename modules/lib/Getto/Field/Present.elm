module Getto.Field.Present exposing
  ( Model
  , Single
  , Between
  , Field
  , Prop
  , init
  , single
  , between
  , string
  , set
  , present
  )
import Getto.Field as Field
import Getto.Field.Form as Form

import Set exposing ( Set )

type alias Model form opts = ( String, form, { opts | isPresent : Bool } )
type alias Single  form opts a = Model (Form.Model   form Attribute a) opts
type alias Between form opts a = Model (Form.Between form Attribute a) opts

type alias Attribute = ()
type alias Field a = Field.Model Attribute a
type alias Prop form a = Form.Prop form Attribute a

type alias Presenter attr a = Field.Model attr a -> Bool


init : String -> value -> String -> Field value
init signature = Field.init signature ()


single : Presenter attr a -> Form.Prop form attr a -> form -> Model (Form.Model form attr a) {}
single f prop model =
  let
    field = model |> Form.at prop
  in
    ( field |> Field.name
    , field |> Form.init prop
    , { isPresent = field |> f
      }
    )

between : { gteq : model -> Model (Form.Model form attr a) opts, lteq : model -> Model (Form.Model form attr a) opts } -> String -> model -> Model (Form.Between form attr a) {}
between units name model =
  let
    gteq = model |> units.gteq
    lteq = model |> units.lteq

    isPresent (_,_,opts) = opts.isPresent
    getForm   (_,form,_) = form
  in
    ( name
    , { gteq = gteq |> getForm
      , lteq = lteq |> getForm
      }
    , { isPresent = ( gteq |> isPresent ) || ( lteq |> isPresent )
      }
    )


string : Presenter attr String
string = present (String.isEmpty >> not)

set : Presenter attr (Set a)
set = present (Set.isEmpty >> not)

present : (a -> Bool) -> Presenter attr a
present f = Field.value >> f
