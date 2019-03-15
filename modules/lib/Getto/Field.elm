module Getto.Field exposing
  ( Model
  , init
  , id
  , name
  , value
  , pair
  , set
  , toggle
  , attribute
  , setAttribute
  )

import Set exposing ( Set )

type Model attr value = Model attr
  { id    : String
  , name  : String
  , value : value
  }

init : String -> attr -> value -> String -> Model attr value
init parentId attr defaultValue fieldName = Model attr
  { id    = parentId ++ "-" ++ fieldName
  , name  = fieldName
  , value = defaultValue
  }


id : Model attr value -> String
id (Model _ model) = model.id

name : Model attr value -> String
name (Model _ model) = model.name

value : Model attr value -> value
value (Model _ model) = model.value

pair : Model attr value -> ( String, value )
pair model = ( model |> id, model |> value )


set : value -> Model attr value -> Model attr value
set val (Model attr model) = Model attr { model | value = val }

toggle : comparable -> Model attr (Set comparable) -> Model attr (Set comparable)
toggle val (Model attr model) =
  Model attr
    { model
    | value =
      if model.value |> Set.member val
        then model.value |> Set.remove val
        else model.value |> Set.insert val
    }

attribute : Model attr value -> attr
attribute (Model attr _) = attr

setAttribute : attr -> Model attr value -> Model attr value
setAttribute attr (Model _ model) = Model attr model
