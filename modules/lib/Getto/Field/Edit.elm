module Getto.Field.Edit exposing
  ( Model
  , State(..)
  , init
  , fields
  , update
  , isStatic
  , response
  , filter
  , encode
  , decode
  , toStatic
  , toEdit
  , commit
  , change
  , put
  , expose
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
  = Static String
  | Edit   String (Conflict.Form form a) (Conflict.State a) (List String)

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

isStatic : Model response fields -> Bool
isStatic (Model state _) = state == StaticState

response : Model response fields -> Maybe response
response (Model state _) =
  case state of
    StaticState   -> Nothing
    EditState _ r -> r |> Just

filter : (Param a -> Encode.Value) -> ( String, Maybe a, Field.Model attr a ) -> Maybe ( String, Encode.Value )
filter encoder (fieldName,value,field) =
  let
    formValue = field |> Field.value

    isSame val =
      if val == formValue
        then Nothing
        else Just val
  in
    value |> Maybe.andThen isSame |> Maybe.map
      (\val ->
        ( fieldName, { from = val, to = formValue } |> encoder )
      )

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

toStatic : Model response fields -> Model response fields
toStatic (Model _ model) = Model StaticState model

toEdit : (response -> Model response fields -> Model response fields) -> HttpView.Model response -> Model response fields -> Model response fields
toEdit edit http (Model state model) =
  case http |> HttpView.response of
    Nothing  -> Model state model
    Just res -> Model (EditState False res) model |> edit res

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


expose : (Maybe response -> response -> Bool) -> Model response fields -> Maybe response -> Validate.Model (Conflict.Form form a) -> State form a
expose isDifferentResponse (Model state _) res model =
  let
    (fieldName,validateForm,errors) = model |> Validate.expose
  in
    case state of
      StaticState -> Static fieldName
      EditState isCommit last ->
        if isCommit && ( last |> isDifferentResponse res )
          then Static fieldName
          else Edit   fieldName validateForm ( validateForm.field |> Conflict.state ) errors
