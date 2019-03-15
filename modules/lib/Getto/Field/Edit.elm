module Getto.Field.Edit exposing
  ( Model
  , State(..)
  , AggregateState(..)
  , init
  , fields
  , update
  , isStatic
  , response
  , param
  , encode
  , decode
  , edit
  , cancel
  , commit
  , change
  , put
  , expose
  , aggregate
  )
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict

import Json.Encode as Encode
import Json.Decode as Decode

type Model response fields = Model (EditState response) fields

type EditState response
  = StaticState
  | EditState Bool response

type State form a
  = Static String a
  | Edit   String (Conflict.Form form a) (Conflict.State a) (List String)

type AggregateState
  = Same
  | HasError
  | HasModified

type alias Param a =
  { from : a
  , to   : a
  }


init : fields -> Model response fields
init = Model StaticState

fields : Model response fields -> fields
fields (Model _ model) = model

update : (fields -> fields) -> Model response fields -> Model response fields
update f (Model state model) = Model state (model |> f)

isStatic : (response -> response -> Bool) -> response -> Model response fields -> Bool
isStatic isDifferentResponse res (Model state _) =
  case state of
    StaticState -> True
    EditState isCommit last -> isCommit && ( last |> isDifferentResponse res )

response : Model response fields -> Maybe response
response (Model state _) =
  case state of
    StaticState   -> Nothing
    EditState _ r -> r |> Just

param : a -> a -> Maybe (Param a)
param lastValue formValue =
  if lastValue == formValue
    then Nothing
    else Just
      { from = lastValue
      , to   = formValue
      }

encode : (response -> Encode.Value) -> (fields -> Encode.Value) -> Model response fields -> Encode.Value
encode encodeResponse encodeModel (Model state model) =
  case state of
    StaticState -> [ ( "state", "static" |> Encode.string ) ] |> Encode.object
    EditState _ res ->
      [ ( "state",   "edit" |> Encode.string )
      , ( "response", res   |> encodeResponse )
      , ( "model",    model |> encodeModel )
      ] |> Encode.object

decode : Decode.Decoder response -> (Decode.Value -> Model response fields -> Model response fields) -> Decode.Value -> Model response fields -> Model response fields
decode decodeResponse decodeModel value (Model state model) =
  case
    ( value |> decodeValue "state"    Decode.string
    , value |> decodeValue "response" decodeResponse
    , value |> decodeValue "model"    Decode.value
    )
  of
    ( Just "edit", Just res, Just val ) -> Model (EditState False res) model |> decodeModel val
    _                                   -> Model state model

decodeValue : String -> Decode.Decoder a -> Decode.Value -> Maybe a
decodeValue key decoder = Decode.decodeValue (Decode.at [key] decoder) >> Result.toMaybe

edit : (response -> Model response fields -> Model response fields) -> HttpView.Model response -> Model response fields -> Model response fields
edit editFields http (Model state model) =
  case http |> HttpView.response of
    Nothing  -> Model state model
    Just res -> Model (EditState False res) model |> editFields res

cancel : Model response fields -> Model response fields
cancel (Model _ model) = Model StaticState model

commit : Model response fields -> Model response fields
commit (Model state model) =
  case state of
    EditState _ res -> Model (EditState True res) model
    _               -> Model state model

change : Model response fields -> Model response fields
change (Model state model) =
  case state of
    EditState _ res -> Model (EditState False res) model
    _               -> Model state model

put : HttpView.Migration annonymous -> Model response fields -> Model response fields
put mig =
  if mig |> HttpView.isConflict
    then change
    else identity


expose : Bool -> Conflict.Model form a -> State form a
expose isStaticState model =
  let
    (fieldName,(value,validateForm),errors) = model |> Conflict.expose
  in
    if isStaticState
      then Static fieldName value
      else Edit   fieldName validateForm ( validateForm.field |> Conflict.state ) errors

aggregate : List Bool -> List String -> AggregateState
aggregate modifieds errors =
  if errors |> List.isEmpty |> not
    then HasError
    else
      if modifieds |> List.any identity
        then HasModified
        else Same
