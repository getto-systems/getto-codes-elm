module Getto.Field.Validate exposing
  ( Model
  , Init
  , init
  , expose
  , nothing
  , blank
  , empty
  , validate
  )
import Getto.Field as Field
import Getto.Field.Form as Form

type Model form = Model
  { name   : String
  , errors : List String
  , form   : form
  }

type alias Init form a = ( Form.Prop form a, List (Maybe String) )

init : Init form a -> form -> Model (Form.Model form a)
init (prop,errors) model =
  let
    field = model |> Form.at prop
  in
    Model
      { name   = field  |> Field.name
      , errors = errors |> List.filterMap identity
      , form   = field  |> Form.init prop
      }


expose : Model form -> ( String, List String, form )
expose (Model model) =
  ( model.name
  , model.errors
  , model.form
  )


nothing : String -> Field.Model (Maybe value) -> Maybe String
nothing error = validate error ((==) Nothing)

blank : String -> Field.Model String -> Maybe String
blank error = validate error String.isEmpty

empty : String -> Field.Model (List a) -> Maybe String
empty error = validate error List.isEmpty

validate : String -> (a -> Bool) -> Field.Model a -> Maybe String
validate error f model =
  if model |> Field.value |> f
    then Just error
    else Nothing
