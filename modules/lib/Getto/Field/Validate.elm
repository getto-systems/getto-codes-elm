module Getto.Field.Validate exposing
  ( Model
  , Init
  , init
  , none
  , form
  , errors
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

type alias Init form attr a =
  ( Form.Prop form attr a
  , List (Maybe String)
  )

type alias InitField attr a = Field.Model attr a -> ( Field.Model attr a, List String )

init : InitField attr a -> Init form attr a -> form -> Model (Form.Model form attr a)
init initField (prop,err) model =
  let
    (field,initErrors) = model |> Form.at prop |> initField
  in
    Model
      { name   = field |> Field.name
      , errors = err   |> List.filterMap identity |> List.append initErrors
      , form   = field |> Form.init prop
      }

none : InitField attr a
none field = ( field, [] )


form : Model form -> form
form (Model model) = model.form

errors : Model form -> List String
errors (Model model) = model.errors

expose : Model form -> ( String, form, List String )
expose (Model model) =
  ( model.name
  , model.form
  , model.errors
  )


nothing : String -> Field.Model attr (Maybe value) -> Maybe String
nothing error = validate error ((==) Nothing)

blank : String -> Field.Model attr String -> Maybe String
blank error = validate error String.isEmpty

empty : String -> Field.Model attr (List a) -> Maybe String
empty error = validate error List.isEmpty

validate : String -> (a -> Bool) -> Field.Model attr a -> Maybe String
validate error f model =
  if model |> Field.value |> f
    then Just error
    else Nothing
