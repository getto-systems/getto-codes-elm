module Getto.Field.Validate exposing
  ( View
  , Model
  , Single
  , Field
  , Prop
  , Validater
  , init
  , single
  , nothing
  , blank
  , empty
  , validate
  , compose
  , compose2
  , compose3
  , compose4
  , compose5
  , compose6
  , compose7
  , compose8
  , compose9
  , compose10
  , compose11
  , compose12
  , compose13
  , compose14
  , compose15
  , compose16
  )
import Getto.Field as Field
import Getto.Field.Form as Form

type alias View model =
  { hasError : Bool
  , form     : model
  }

type alias Model form opts = ( String, form, { opts | errors : List String } )
type alias Single form opts a = Model (Form.Model form Attribute a) opts

type alias Attribute = ()
type alias Field a = Field.Model Attribute a
type alias Prop form a = Form.Prop form Attribute a

type alias Validater attr a = Field.Model attr a -> Maybe String

init : String -> value -> String -> Field value
init signature = Field.init signature ()

single : List (Maybe String) -> Form.Prop form attr a -> form -> Model (Form.Model form attr a) {}
single errors prop form =
  let
    field = form |> Form.at prop
  in
    ( field  |> Field.name
    , field  |> Form.init prop
    , { errors = errors |> List.filterMap identity
      }
    )


nothing : String -> Validater attr (Maybe value)
nothing error = validate error ((==) Nothing)

blank : String -> Validater attr String
blank error = validate error String.isEmpty

empty : String -> Validater attr (List a)
empty error = validate error List.isEmpty

validate : String -> (a -> Bool) -> Validater attr a
validate error f model =
  if model |> Field.value |> f
    then Just error
    else Nothing

compose : (Model a opts -> model) -> Model a opts -> View model
compose model a =
  { hasError =
    [ a |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a
  }

compose2 : (Model a opts -> Model b opts -> model) -> Model a opts -> Model b opts -> View model
compose2 model a b =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b
  }

compose3 : (Model a opts -> Model b opts -> Model c opts -> model) -> Model a opts -> Model b opts -> Model c opts -> View model
compose3 model a b c =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c
  }

compose4 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> View model
compose4 model a b c d =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d
  }

compose5 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> View model
compose5 model a b c d e =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e
  }

compose6 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> View model
compose6 model a b c d e f =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f
  }

compose7 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> View model
compose7 model a b c d e f g =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g
  }

compose8 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> View model
compose8 model a b c d e f g h =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h
  }

compose9 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> View model
compose9 model a b c d e f g h i =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i
  }

compose10 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> View model
compose10 model a b c d e f g h i j =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j
  }

compose11 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> View model
compose11 model a b c d e f g h i j k =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    , k |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j k
  }

compose12 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> View model
compose12 model a b c d e f g h i j k l =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    , k |> toErrors
    , l |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j k l
  }

compose13 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> View model
compose13 model a b c d e f g h i j k l m =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    , k |> toErrors
    , l |> toErrors
    , m |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j k l m
  }

compose14 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> View model
compose14 model a b c d e f g h i j k l m n =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    , k |> toErrors
    , l |> toErrors
    , m |> toErrors
    , n |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j k l m n
  }

compose15 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> Model o opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> Model o opts -> View model
compose15 model a b c d e f g h i j k l m n o =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    , k |> toErrors
    , l |> toErrors
    , m |> toErrors
    , n |> toErrors
    , o |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j k l m n o
  }

compose16 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> Model o opts -> Model p opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> Model o opts -> Model p opts -> View model
compose16 model a b c d e f g h i j k l m n o p =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    , k |> toErrors
    , l |> toErrors
    , m |> toErrors
    , n |> toErrors
    , o |> toErrors
    , p |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j k l m n o p
  }

toErrors : Model form m -> List String
toErrors (_,_,m) = m.errors
