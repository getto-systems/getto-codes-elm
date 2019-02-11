module Getto.Field exposing
  ( Model
  , Update
  , init
  , id
  , name
  , value
  , input
  , change
  , nothing
  , blank
  , empty
  , validate
  )

type Model value = Model
  { id    : String
  , name  : String
  , input : value
  , value : value
  }

type alias Update value = value -> Model value -> Model value

init : String -> String -> value -> Model value
init parentId fieldName defaultValue = Model
  { id    = parentId ++ "-" ++ fieldName
  , name  = fieldName
  , input = defaultValue
  , value = defaultValue
  }


id : Model value -> String
id (Model model) = model.id

name : Model value -> String
name (Model model) = model.name

value : Model value -> value
value (Model model) = model.value


input : Update value
input val (Model model) = Model { model | input = val }

change : Update value
change val (Model model) = Model { model | input = val, value = val }


nothing : String -> Model (Maybe value) -> Maybe String
nothing error = validate error ((==) Nothing)

blank : String -> Model String -> Maybe String
blank error = validate error String.isEmpty

empty : String -> Model (List a) -> Maybe String
empty error = validate error List.isEmpty

validate : String -> (a -> Bool) -> Model a -> Maybe String
validate error f (Model model) =
  if model.input |> f
    then Just error
    else Nothing
