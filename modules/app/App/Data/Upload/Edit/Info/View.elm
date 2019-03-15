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
  { name  = "name"  |> Field.init signature Conflict.none ""
  , memo  = "memo"  |> Field.init signature Conflict.none ""
  , age   = "age"   |> Field.init signature Conflict.none ""
  , email = "email" |> Field.init signature Conflict.none ""
  , tel   = "tel"   |> Field.init signature Conflict.none ""
  }
  |> Edit.init

params : HttpView.Model Data.Response -> Form -> Encode.Value
params get =
  case get |> HttpView.response of
    Nothing  -> always Encode.null
    Just res -> Edit.fields >>
      (\fields ->
        let
          encode encoder values =
            [ ( "from", values.from |> encoder )
            , ( "to",   values.to   |> encoder )
            ] |> Encode.object

          map_name_param getter encoder =
            Field.name_value >>
            Tuple.mapSecond
              ( Edit.param ( res |> getter )
                >> Maybe.map (encode encoder)
              )
        in
          [ fields.name  |> map_name_param get_name  Encode.string
          , fields.memo  |> map_name_param get_memo  Encode.string
          , fields.age   |> map_name_param get_age   Encode.string
          , fields.email |> map_name_param get_email Encode.string
          , fields.tel   |> map_name_param get_tel   Encode.string
          ]
          |> List.filterMap (\(k,v) -> v |> Maybe.map (\val -> ( k, val )))
          |> Encode.object
      )

encodeForm : Form -> Encode.Value
encodeForm = Edit.encode Data.encodeResponse encodeFields

encodeFields : Fields -> Encode.Value
encodeFields fields =
  [ fields.name  |> Field.name_value |> Tuple.mapSecond Encode.string
  , fields.memo  |> Field.name_value |> Tuple.mapSecond Encode.string
  , fields.age   |> Field.name_value |> Tuple.mapSecond Encode.string
  , fields.email |> Field.name_value |> Tuple.mapSecond Encode.string
  , fields.tel   |> Field.name_value |> Tuple.mapSecond Encode.string
  ] |> Encode.object

decodeForm : Decode.Value -> Form -> Form
decodeForm = Edit.decode Data.decodeResponse decodeFields

decodeFields : Decode.Value -> Form -> Form
decodeFields value form =
  let
    decode field decoder =
      value
      |> Decode.decodeValue
        (Decode.at [form |> Edit.fields |> field |> Field.name] decoder)
      |> Result.toMaybe
  in
    form
    |> Form.setIf name_  ( decode .name  Decode.string )
    |> Form.setIf memo_  ( decode .memo  Decode.string )
    |> Form.setIf age_   ( decode .age   Decode.string )
    |> Form.setIf email_ ( decode .email Decode.string )
    |> Form.setIf tel_   ( decode .tel   Decode.string )

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
