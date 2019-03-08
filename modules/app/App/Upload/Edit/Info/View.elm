module GettoUpload.App.Upload.Edit.Info.View exposing
  ( Form
  , View
  , Prop
  , Response
  , response
  , init
  , pairs
  , params
  , encodeForm
  , decodeForm
  , edit
  , view
  )
import GettoUpload.App.Upload.Edit.Data.View as Data
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

type alias View =
  { isStatic : Bool
  , hasError : Bool
  , state    : HttpView.State
  , response : Maybe Data.Response
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

get_name  = .info >> .name
get_memo  = .info >> .memo
get_age   = .info >> .age >> String.fromInt
get_email = .info >> .email
get_tel   = .info >> .tel


init : String -> Form
init signature =
  { name  = Field.init signature "name"  Conflict.none ""
  , memo  = Field.init signature "memo"  Conflict.none ""
  , age   = Field.init signature "age"   Conflict.none ""
  , email = Field.init signature "email" Conflict.none ""
  , tel   = Field.init signature "tel"   Conflict.none ""
  }
  |> Edit.init

pairs : Form -> List ( String, String )
pairs = Edit.fields >>
  (\fields ->
    [ fields.name  |> Field.pair
    , fields.memo  |> Field.pair
    , fields.age   |> Field.pair
    , fields.email |> Field.pair
    , fields.tel   |> Field.pair
    ]
  )

params : HttpView.Model Data.Response -> Form -> Encode.Value
params get = Edit.fields >>
  (\fields ->
    let
      body = get |> HttpView.response |> Maybe.map HttpView.body

      encode encoder values =
        [ ( "from", values.from |> encoder )
        , ( "to",   values.to   |> encoder )
        ] |> Encode.object
    in
      [ ( "name",  body |> Maybe.map get_name,  fields.name  ) |> Edit.filter (encode Encode.string)
      , ( "memo",  body |> Maybe.map get_memo,  fields.memo  ) |> Edit.filter (encode Encode.string)
      , ( "age",   body |> Maybe.map get_age,   fields.age   ) |> Edit.filter (encode Encode.string)
      , ( "email", body |> Maybe.map get_email, fields.email ) |> Edit.filter (encode Encode.string)
      , ( "tel",   body |> Maybe.map get_tel,   fields.tel   ) |> Edit.filter (encode Encode.string)
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
  let
    body = res |> HttpView.body
  in
    form
    |> Form.set name_  (body |> get_name)
    |> Form.set memo_  (body |> get_memo)
    |> Form.set age_   (body |> get_age)
    |> Form.set email_ (body |> get_email)
    |> Form.set tel_   (body |> get_tel)


view : HttpView.Model Data.Response -> Form -> View
view http form =
  let
    error = "conflict"

    blank = Validate.blank "blank"

    res = http |> HttpView.response

    data =
      ( form |> Edit.response |> Maybe.map HttpView.body
      , res  |> Maybe.map HttpView.body
      )

    fields = form |> Edit.fields

    model =
      { name  = form |> Conflict.init error data ( get_name, ( name_,  [ fields.name |> blank ] ) )
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

    expose = Edit.expose Data.isDifferentResponse form res
  in
    { isStatic = form   |> Edit.isStatic
    , hasError = errors |> List.isEmpty |> not
    , state    = http   |> HttpView.state
    , response = res
    , form     =
      { name  = model.name  |> expose
      , memo  = model.memo  |> expose
      , age   = model.age   |> expose
      , email = model.email |> expose
      , tel   = model.tel   |> expose
      }
    }
