module GettoUpload.App.Data.Upload.Edit.Info.View exposing
  ( Form
  , Fields
  , View
  , Prop
  , Response
  , response
  , init
  , params
  , encodeForm
  , decodeForm
  , edit
  , view
  )
import GettoUpload.App.Data.Upload.Edit.Data.View as Data
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Edit as Edit
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict
import Getto.Http.Header.Decode as HeaderDecode

import Json.Encode as Encode
import Json.Decode as Decode

type alias Prop  a = Conflict.Prop Form a
type alias Field a = Conflict.Field a

type alias Form = Edit.Model Data.Response Fields
type alias Fields =
  { name  : Field String
  , memo  : Field String
  , age   : Field String
  , email : Field String
  , tel   : Field String
  }

type alias View = HttpView.Model Data.Response -> Maybe
  { isStatic : Bool
  , state    : Edit.AggregateState
  , form :
    { name  : Edit.State Form String
    , memo  : Edit.State Form String
    , age   : Edit.State Form String
    , email : Edit.State Form String
    , tel   : Edit.State Form String
    }
  }

type alias Response = HttpView.Response () ()


response : HttpView.ResponseDecoder Response
response = HttpView.decoder
  { header = HeaderDecode.succeed ()
  , body   = Decode.succeed ()
  }


name_  = Form.prop (Edit.fields >> .name)  (\v -> Edit.update (\m -> { m | name  = v }) )
memo_  = Form.prop (Edit.fields >> .memo)  (\v -> Edit.update (\m -> { m | memo  = v }) )
age_   = Form.prop (Edit.fields >> .age)   (\v -> Edit.update (\m -> { m | age   = v }) )
email_ = Form.prop (Edit.fields >> .email) (\v -> Edit.update (\m -> { m | email = v }) )
tel_   = Form.prop (Edit.fields >> .tel)   (\v -> Edit.update (\m -> { m | tel   = v }) )

get_name  = HttpView.body >> .info >> .name
get_memo  = HttpView.body >> .info >> .memo
get_age   = HttpView.body >> .info >> .age >> String.fromInt
get_email = HttpView.body >> .info >> .email
get_tel   = HttpView.body >> .info >> .tel


init : String -> Form
init signature =
  { name  = Field.init signature "name"  Conflict.none ""
  , memo  = Field.init signature "memo"  Conflict.none ""
  , age   = Field.init signature "age"   Conflict.none ""
  , email = Field.init signature "email" Conflict.none ""
  , tel   = Field.init signature "tel"   Conflict.none ""
  }
  |> Edit.init

params : HttpView.Model Data.Response -> Form -> Encode.Value
params get = Edit.fields >>
  (\fields ->
    let
      res = get |> HttpView.response

      encode encoder values =
        [ ( "from", values.from |> encoder )
        , ( "to",   values.to   |> encoder )
        ] |> Encode.object
    in
      [ ( "name",  res |> Maybe.map get_name,  fields.name  ) |> Edit.filter (encode Encode.string)
      , ( "memo",  res |> Maybe.map get_memo,  fields.memo  ) |> Edit.filter (encode Encode.string)
      , ( "age",   res |> Maybe.map get_age,   fields.age   ) |> Edit.filter (encode Encode.string)
      , ( "email", res |> Maybe.map get_email, fields.email ) |> Edit.filter (encode Encode.string)
      , ( "tel",   res |> Maybe.map get_tel,   fields.tel   ) |> Edit.filter (encode Encode.string)
      ]
      |> List.filterMap identity
      |> Encode.object
  )

encodeForm : Form -> Encode.Value
encodeForm = Edit.encode Data.encodeResponse encodeFields

encodeFields : Fields -> Encode.Value
encodeFields fields =
  [ ( "name",  fields.name  |> Field.value |> Encode.string )
  , ( "memo",  fields.memo  |> Field.value |> Encode.string )
  , ( "age",   fields.age   |> Field.value |> Encode.string )
  , ( "email", fields.email |> Field.value |> Encode.string )
  , ( "tel",   fields.tel   |> Field.value |> Encode.string )
  ] |> Encode.object

decodeForm : Decode.Value -> Form -> Form
decodeForm = Edit.decode Data.decodeResponse decodeFields

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

edit : Data.Response -> Form -> Form
edit res form =
  form
  |> Form.set name_  (res |> get_name)
  |> Form.set memo_  (res |> get_memo)
  |> Form.set age_   (res |> get_age)
  |> Form.set email_ (res |> get_email)
  |> Form.set tel_   (res |> get_tel)


view : Form -> View
view form = HttpView.response >> Maybe.map
  (\res ->
    let
      error = "conflict"

      blank = Validate.blank "blank"

      data =
        ( form |> Edit.response
        , res
        )

      fields = form |> Edit.fields

      model =
        { name  = form |> Conflict.init error data ( get_name, ( name_,  [ fields.name |> blank ] ) )
        , memo  = form |> Conflict.init error data ( get_memo, ( memo_,  [] ) )
        , age   = form |> Conflict.init error data ( get_age,  ( age_,   [] ) )
        , email = form |> Conflict.init error data ( get_email,( email_, [] ) )
        , tel   = form |> Conflict.init error data ( get_tel,  ( tel_,   [] ) )
        }

      modifieds =
        [ model.name  |> Conflict.modified
        , model.memo  |> Conflict.modified
        , model.age   |> Conflict.modified
        , model.email |> Conflict.modified
        , model.tel   |> Conflict.modified
        ]

      errors = List.concat
        [ model.name  |> Conflict.form |> Validate.errors
        , model.memo  |> Conflict.form |> Validate.errors
        , model.age   |> Conflict.form |> Validate.errors
        , model.email |> Conflict.form |> Validate.errors
        , model.tel   |> Conflict.form |> Validate.errors
        ]

      isStatic = form |> Edit.isStatic Data.isDifferentResponse res
    in
      { isStatic = isStatic
      , state    = errors |> Edit.aggregate modifieds
      , form =
        { name  = model.name  |> Edit.expose isStatic
        , memo  = model.memo  |> Edit.expose isStatic
        , age   = model.age   |> Edit.expose isStatic
        , email = model.email |> Edit.expose isStatic
        , tel   = model.tel   |> Edit.expose isStatic
        }
      }
  )
