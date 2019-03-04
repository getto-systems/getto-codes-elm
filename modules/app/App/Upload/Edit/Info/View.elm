module GettoUpload.App.Upload.Edit.Info.View exposing
  ( Form
  , View
  , Prop
  , State(..)
  , Response
  , init
  , encodeForm
  , decodeForm
  , toStatic
  , toEdit
  , toCommit
  , changed
  , view
  )
import GettoUpload.App.Upload.Edit.Data.View as Data
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict

import File exposing ( File )
import Set exposing ( Set )
import Json.Encode as Encode
import Json.Decode as Decode

type alias Prop      a = Conflict.Prop Form a
type alias Field     a = Conflict.Field a
type alias ViewModel a = Validate.Model (Conflict.Form Form a)

type alias Form =
  { state : EditState
  , name  : Field String
  , memo  : Field String
  , age   : Field String
  , email : Field String
  , tel   : Field String
  }

type alias View =
  { isStatic : Bool
  , hasError : Bool
  , state    : HttpView.State
  , response : Maybe Data.Response
  , form :
    { name  : State String
    , memo  : State String
    , age   : State String
    , email : State String
    , tel   : State String
    }
  }

type alias Response = HttpView.Response () ()

type EditState
  = StaticState
  | EditState Bool Data.Response

type State a
  = Static String
  | Edit   String (Conflict.Form Form a) (Conflict.State a) (List String)

name_     = Form.prop .name     (\v m -> { m | name     = v })
memo_     = Form.prop .memo     (\v m -> { m | memo     = v })
age_      = Form.prop .age      (\v m -> { m | age      = v })
email_    = Form.prop .email    (\v m -> { m | email    = v })
tel_      = Form.prop .tel      (\v m -> { m | tel      = v })

init : String -> Form
init signature =
  { state    = StaticState
  , name     = Field.init signature "name"     Conflict.none ""
  , memo     = Field.init signature "memo"     Conflict.none ""
  , age      = Field.init signature "age"      Conflict.none ""
  , email    = Field.init signature "email"    Conflict.none ""
  , tel      = Field.init signature "tel"      Conflict.none ""
  }

encodeForm : Form -> Encode.Value
encodeForm form =
  case form.state of
    StaticState -> [ ( "state", "static" |> Encode.string ) ] |> Encode.object
    EditState _ res ->
      [ ( "state",  "edit" |> Encode.string )
      , ( "response", res  |> Data.encodeResponse )
      , ( "form",     form |> encodeFields )
      ] |> Encode.object

encodeFields : Form -> Encode.Value
encodeFields form =
  [ ( "name",  form.name  |> Field.value |> Encode.string )
  , ( "memo",  form.memo  |> Field.value |> Encode.string )
  , ( "age",   form.age   |> Field.value |> Encode.string )
  , ( "email", form.email |> Field.value |> Encode.string )
  , ( "tel",   form.tel   |> Field.value |> Encode.string )
  ] |> Encode.object

decodeForm : Decode.Value -> Form -> Form
decodeForm value =
  case
    ( value |> decode "state"    Decode.string
    , value |> decode "response" Data.decodeResponse
    , value |> decode "form"     Decode.value
    )
  of
    ( Just "edit", Just res, Just form ) -> toEdit res >> decodeFields form
    _ -> identity

decodeFields : Decode.Value -> Form -> Form
decodeFields value form =
  form
  |> Form.setIf name_  ( value |> decode "name"  Decode.string )
  |> Form.setIf memo_  ( value |> decode "memo"  Decode.string )
  |> Form.setIf age_   ( value |> decode "age"   Decode.string )
  |> Form.setIf email_ ( value |> decode "email" Decode.string )
  |> Form.setIf tel_   ( value |> decode "tel"   Decode.string )

decode : String -> Decode.Decoder a -> Decode.Value -> Maybe a
decode key decoder = Decode.decodeValue (Decode.at [key] decoder) >> Result.toMaybe

toStatic : Form -> Form
toStatic form = { form | state = StaticState }

toEdit : Data.Response -> Form -> Form
toEdit res form =
  let
    body = res |> HttpView.body
  in
    { form | state = EditState False res }
    |> Form.set name_   body.info.name
    |> Form.set memo_   body.info.memo
    |> Form.set age_   (body.info.age |> String.fromInt)
    |> Form.set email_  body.info.email
    |> Form.set tel_    body.info.tel

toCommit : Form -> Form
toCommit form =
  case form.state of
    EditState _ res -> { form | state = EditState True res }
    _ -> form

changed : Form -> Form
changed form =
  case form.state of
    EditState _ res -> { form | state = EditState False res }
    _ -> form


view : HttpView.Model Data.Response -> Form -> View
view http form =
  let
    error = "conflict"

    res = http |> HttpView.response

    data =
      ( case form.state of
        StaticState -> Nothing
        EditState _ r -> r |> HttpView.body |> Just

      , res |> Maybe.map HttpView.body
      )
    model =
      { name  = ( .info >> .name,                  ( name_,  [ form.name |> Validate.blank "blank" ] ) )
      , memo  = ( .info >> .memo,                  ( memo_,  [] ) )
      , age   = ( .info >> .age >> String.fromInt, ( age_,   [] ) )
      , email = ( .info >> .email,                 ( email_, [] ) )
      , tel   = ( .info >> .tel,                   ( tel_,   [] ) )
      }
    result =
      { name     = form |> Conflict.init error data model.name
      , memo     = form |> Conflict.init error data model.memo
      , age      = form |> Conflict.init error data model.age
      , email    = form |> Conflict.init error data model.email
      , tel      = form |> Conflict.init error data model.tel
      }
    errors = List.concat
      [ result.name     |> Validate.errors
      , result.memo     |> Validate.errors
      , result.age      |> Validate.errors
      , result.email    |> Validate.errors
      , result.tel      |> Validate.errors
      ]
  in
    { isStatic = (form.state == StaticState)
    , hasError = errors |> List.isEmpty |> not
    , state    = http |> HttpView.state
    , response = res
    , form     =
      { name     = result.name     |> expose form.state res
      , memo     = result.memo     |> expose form.state res
      , age      = result.age      |> expose form.state res
      , email    = result.email    |> expose form.state res
      , tel      = result.tel      |> expose form.state res
      }
    }

expose : EditState -> Maybe Data.Response -> Validate.Model (Conflict.Form Form a) -> State a
expose st res model =
  case model |> Validate.expose of
    (fieldName,form,errors) ->
      case st of
        StaticState -> Static fieldName
        EditState isCommit last ->
          if isCommit && ( last |> isDifferentResponse res )
            then Static fieldName
            else Edit   fieldName form ( form.field |> Conflict.state ) errors

isDifferentResponse : Maybe Data.Response -> Data.Response -> Bool
isDifferentResponse data last =
  case data of
    Nothing  -> False
    Just res -> ( res |> Data.etag ) /= ( last |> Data.etag )
