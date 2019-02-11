module Getto.Field.View exposing
  ( Model
  , init
  , id
  , name
  , value
  , errors
  , isError
  )
import Getto.Field as Field

type Model value = Model (List String) (Field.Model value)

init : List String -> Field.Model value -> Model value
init = Model

id : Model value -> String
id (Model _ field) = field |> Field.id

name : Model value -> String
name (Model _ field) = field |> Field.name

value : Model value -> value
value (Model _ field) = field |> Field.value

errors : Model value -> List String
errors (Model err _) = err

isError : Model value -> Bool
isError = errors >> List.isEmpty >> not
