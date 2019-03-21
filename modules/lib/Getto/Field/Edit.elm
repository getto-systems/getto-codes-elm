module Getto.Field.Edit exposing
  ( Form
  , View
  , AggregateState(..)
  , form
  , fields
  , update
  , encode
  , decode
  , edit
  , cancel
  , commit
  , change
  , options
  , view
  )

import Getto.Apply as Apply
import Getto.Field as Field
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict

import Json.Encode as Encode
import Json.Decode as Decode

type alias Model form opts = Conflict.Model form opts

type alias View model =
  { isStatic : Bool
  , state    : AggregateState
  , form     : model
  }

type Form response fields = Form (State response) fields

type alias Options response fields =
  { error       : String
  , form        : Form response fields
  , last        : response
  , isConflict  : Bool
  , isDifferent : response -> response -> Bool
  }

type State response
  = Static
  | Edit Bool response

type AggregateState
  = Same
  | HasError
  | HasModified


form : fields -> Form response fields
form = Form Static

fields : Form response fields -> fields
fields (Form _ model) = model

update : (fields -> fields) -> Form response fields -> Form response fields
update f (Form state model) = Form state (model |> f)

encode : (response -> Encode.Value) -> (fields -> Encode.Value) -> Form response fields -> Encode.Value
encode encodeResponse encodeModel (Form state model) =
  case state of
    Static -> [ ( "state", "static" |> Encode.string ) ] |> Encode.object
    Edit _ res ->
      [ ( "state",   "edit" |> Encode.string )
      , ( "response", res   |> encodeResponse )
      , ( "model",    model |> encodeModel )
      ] |> Encode.object

decode : Decode.Decoder response -> (Decode.Value -> Form response fields -> Form response fields) -> Decode.Value -> Form response fields -> Form response fields
decode decodeResponse decodeModel value (Form state model) =
  case
    ( value |> decodeValue "state"    Decode.string
    , value |> decodeValue "response" decodeResponse
    , value |> decodeValue "model"    Decode.value
    )
  of
    ( Just "edit", Just res, Just val ) -> Form (Edit False res) model |> decodeModel val
    _                                   -> Form state model

decodeValue : String -> Decode.Decoder a -> Decode.Value -> Maybe a
decodeValue key decoder = Decode.decodeValue (Decode.at [key] decoder) >> Result.toMaybe

edit : (response -> Form response fields -> Form response fields) -> Maybe response -> Form response fields -> Form response fields
edit editFields value (Form state model) =
  case value of
    Nothing  -> Form state model
    Just res -> Form (Edit False res) model |> editFields res

cancel : Form response fields -> Form response fields
cancel (Form _ model) = Form Static model

commit : Form response fields -> Form response fields
commit (Form state model) =
  case state of
    Edit _ res -> Form (Edit True res) model
    _          -> Form state model

change : Form response fields -> Form response fields
change (Form state model) =
  case state of
    Edit _ res -> Form (Edit False res) model
    _          -> Form state model

options : Options response fields -> ( Bool, Conflict.Options (Form response fields) response )
options opts =
  let
    (Form state _) = opts.form

    response =
      case state of
        Static -> Nothing
        Edit _ res -> res |> Just

    isDifferent = response |> Maybe.map (opts.last |> opts.isDifferent) |> Maybe.withDefault False

    isStatic =
      case state of
        Static -> True
        Edit isCommit _ ->
          opts.isConflict ||
          ( isCommit && isDifferent )
  in
    ( isStatic
    , { error = opts.error
      , form  = opts.form
      , response =
        { first = response
        , last  = opts.last
        }
      }
    )

view : ( Bool, Conflict.View model ) -> View model
view (isStatic,conflictView) =
  { isStatic = isStatic
  , form = conflictView.form
  , state =
    if conflictView.hasError
      then HasError
      else
        if conflictView.hasModified
          then HasModified
          else Same
  }
