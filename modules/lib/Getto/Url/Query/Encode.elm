module Getto.Url.Query.Encode exposing
  ( Value
  , string
  , int
  , bool
  , list
  , object
  , empty
  , encode
  , toName
  )

import Url

type Value
  = ValueEntry String
  | ValueBool Bool
  | ValueGroup (List ( String, Value ))

string : String -> Value
string = Url.percentEncode >> ValueEntry

int : Int -> Value
int = String.fromInt >> ValueEntry

bool : Bool -> Value
bool = ValueBool

list : (a -> Value) -> List a -> Value
list encoder = List.map (\value -> ("", value |> encoder)) >> ValueGroup

object : List ( String, Value ) -> Value
object = List.map (\(name, value) -> ( name |> Url.percentEncode, value ) ) >> ValueGroup

empty : Value
empty = [] |> object

encode : Value -> String
encode value = toQuery <|
  case value of
    ValueEntry val  -> [ val ]
    ValueBool  val  -> []
    ValueGroup vals ->
      vals
      |> List.concatMap (flatten [])
      |> List.map toPair

flatten : List String -> ( String, Value ) -> List ( List String, Maybe String )
flatten parents (name, value) =
  let
    current = parents ++ [ name ]
  in
    case value of
      ValueEntry val  -> [ ( current, Just val ) ]
      ValueBool  val  -> if val then [ ( current, Nothing ) ] else []
      ValueGroup vals -> vals |> List.concatMap (flatten current)

toName : List String -> String
toName names =
  case names of
    [] -> ""
    head :: tail ->
      head ++
      ( tail
      |> List.map (\tip -> "[" ++ tip ++ "]")
      |> String.join ""
      )

toPair : ( List String, Maybe String ) -> String
toPair (names,val) =
  (names |> toName) ++
  (case val of
    Just value -> "=" ++ value
    Nothing    -> ""
  )

toQuery : List String -> String
toQuery params =
  if params |> List.isEmpty
    then ""
    else "?" ++ (params |> String.join "&")
