module Getto.Field exposing
  ( Model
  , init
  , name
  , value
  , change
  , nothing
  , blank
  , empty
  , validate
  )

type Model value = Model
  { name  : String
  , value : value
  }

init : String -> value -> Model value
init fieldName defaultValue = Model
  { name  = fieldName
  , value = defaultValue
  }


name : Model value -> String
name (Model model) = model.name

value : Model value -> value
value (Model model) = model.value


change : value -> Model value -> Model value
change val (Model model) = Model { model | value = val }


nothing : String -> Model (Maybe value) -> Maybe String
nothing error = validate error ((==) Nothing)

blank : String -> Model String -> Maybe String
blank error = validate error String.isEmpty

empty : String -> Model (List a) -> Maybe String
empty error = validate error List.isEmpty

validate : String -> (a -> Bool) -> Model a -> Maybe String
validate error f (Model model) =
  if model.value |> f
    then Just error
    else Nothing
