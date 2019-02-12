module Getto.Field exposing
  ( Model
  , init
  , id
  , name
  , value
  , update
  , check
  , nothing
  , blank
  , empty
  , validate
  )

import Set exposing ( Set )

type Model value = Model
  { id    : String
  , name  : String
  , value : value
  }

init : String -> String -> value -> Model value
init parentId fieldName defaultValue = Model
  { id    = parentId ++ "-" ++ fieldName
  , name  = fieldName
  , value = defaultValue
  }


id : Model value -> String
id (Model model) = model.id

name : Model value -> String
name (Model model) = model.name

value : Model value -> value
value (Model model) = model.value


update : value -> Model value -> Model value
update val (Model model) = Model { model | value = val }

check : comparable -> Bool -> Model (Set comparable) -> Model (Set comparable)
check val checked (Model model) =
  Model
    { model
    | value =
      if checked
        then model.value |> Set.insert val
        else model.value |> Set.remove val
    }


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
