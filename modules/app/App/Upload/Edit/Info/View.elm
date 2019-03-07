module GettoUpload.App.Upload.Edit.Info.View exposing
  ( Form
  , View
  , Prop
  , State(..)
  , Response
  , response
  , init
  , params
  , encodeForm
  , decodeForm
  , toStatic
  , toEdit
  , toCommit
  , changed
  , put
  , view
  )
import GettoUpload.App.Upload.Edit.Data.View as Data
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict
import Getto.Http.Header.Decode as HeaderDecode

import File exposing ( File )

import Set exposing ( Set )
import Json.Encode as Encode
import Json.Decode as Decode

type alias Prop  a = Conflict.Prop Form a
type alias Field a = Conflict.Field a

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

type alias ParamValue a =
  { from : a
  , to   : a
  }


response : HttpView.ResponseDecoder Response
response = HttpView.decoder
  { header = HeaderDecode.succeed ()
  , body   = Decode.succeed ()
  }


name_  = Form.prop .name  (\v m -> { m | name  = v })
memo_  = Form.prop .memo  (\v m -> { m | memo  = v })
age_   = Form.prop .age   (\v m -> { m | age   = v })
email_ = Form.prop .email (\v m -> { m | email = v })
tel_   = Form.prop .tel   (\v m -> { m | tel   = v })

get_name  = .info >> .name
get_memo  = .info >> .memo
get_age   = .info >> .age >> String.fromInt
get_email = .info >> .email
get_tel   = .info >> .tel


init : String -> Form
init signature =
  { state = StaticState
  , name  = Field.init signature "name"  Conflict.none ""
  , memo  = Field.init signature "memo"  Conflict.none ""
  , age   = Field.init signature "age"   Conflict.none ""
  , email = Field.init signature "email" Conflict.none ""
  , tel   = Field.init signature "tel"   Conflict.none ""
  }

params : HttpView.Model Data.Response -> Form -> Encode.Value
params get form =
  let
    body = get |> HttpView.response |> Maybe.map HttpView.body

    encode encoder values =
      [ ( "from", values.from |> encoder )
      , ( "to",   values.to   |> encoder )
      ] |> Encode.object
  in
    [ ( "name",  body |> Maybe.map get_name,  form.name  ) |> filter (encode Encode.string)
    , ( "memo",  body |> Maybe.map get_memo,  form.memo  ) |> filter (encode Encode.string)
    , ( "age",   body |> Maybe.map get_age,   form.age   ) |> filter (encode Encode.string)
    , ( "email", body |> Maybe.map get_email, form.email ) |> filter (encode Encode.string)
    , ( "tel",   body |> Maybe.map get_tel,   form.tel   ) |> filter (encode Encode.string)
    ]
    |> List.filterMap identity
    |> Encode.object

filter : (ParamValue a -> Encode.Value) -> ( String, Maybe a, Field a ) -> Maybe ( String, Encode.Value )
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
    |> Form.set name_  (body |> get_name)
    |> Form.set memo_  (body |> get_memo)
    |> Form.set age_   (body |> get_age)
    |> Form.set email_ (body |> get_email)
    |> Form.set tel_   (body |> get_tel)

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

put : HttpView.Migration Response -> Form -> Form
put mig =
  if mig |> HttpView.isConflict
    then changed
    else identity


view : HttpView.Model Data.Response -> Form -> View
view http form =
  let
    error = "conflict"

    blank = Validate.blank "blank"

    res = http |> HttpView.response

    data =
      ( case form.state of
        StaticState -> Nothing
        EditState _ r -> r |> HttpView.body |> Just

      , res |> Maybe.map HttpView.body
      )
    model =
      { name  = form |> Conflict.init error data ( get_name, ( name_,  [ form.name |> blank ] ) )
      , memo  = form |> Conflict.init error data ( get_memo, ( memo_,  [] ) )
      , age   = form |> Conflict.init error data ( get_age,  ( age_,   [] ) )
      , email = form |> Conflict.init error data ( get_email,( email_, [] ) )
      , tel   = form |> Conflict.init error data ( get_tel,  ( tel_,   [] ) )
      }
    errors = List.concat
      [ model.name  |> Validate.errors
      , model.memo  |> Validate.errors
      , model.age   |> Validate.errors
      , model.email |> Validate.errors
      , model.tel   |> Validate.errors
      ]
  in
    { isStatic = (form.state == StaticState)
    , hasError = errors |> List.isEmpty |> not
    , state    = http |> HttpView.state
    , response = res
    , form     =
      { name  = model.name  |> expose form.state res
      , memo  = model.memo  |> expose form.state res
      , age   = model.age   |> expose form.state res
      , email = model.email |> expose form.state res
      , tel   = model.tel   |> expose form.state res
      }
    }

expose : EditState -> Maybe Data.Response -> Validate.Model (Conflict.Form Form a) -> State a
expose st res model =
  let
    (fieldName,form,errors) = model |> Validate.expose
  in
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
