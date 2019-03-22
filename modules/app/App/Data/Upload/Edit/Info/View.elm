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
import GettoUpload.View.Validate as V

import Getto.Apply as Apply
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Edit as Edit
import Getto.Field.Conflict as Conflict
import Getto.Http.Header.Decode as HeaderDecode

import Json.Encode as Encode
import Json.Decode as Decode

type alias Prop  a = Conflict.Prop Form a
type alias Field a = Conflict.Field a
type alias Single a = Conflict.Single Form {} a

type alias Form = Edit.Form Data.Response Fields
type alias Fields =
  { name  : Field String
  , memo  : Field String
  , age   : Field String
  , email : Field String
  , tel   : Field String
  }

type alias View = Edit.View ViewForm
type alias ViewForm =
  { name  : Single String
  , memo  : Single String
  , age   : Single String
  , email : Single String
  , tel   : Single String
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
  { name  = "name"  |> Conflict.init signature ""
  , memo  = "memo"  |> Conflict.init signature ""
  , age   = "age"   |> Conflict.init signature ""
  , email = "email" |> Conflict.init signature ""
  , tel   = "tel"   |> Conflict.init signature ""
  }
  |> Edit.form

params : HttpView.Model Data.Response -> Form -> Encode.Value
params get =
  case get |> HttpView.response of
    Nothing  -> always Encode.null
    Just res -> Edit.fields >>
      (\fields ->
        let
          encode encoder values =
            [ ( "before", values.before |> encoder )
            , ( "after",  values.after  |> encoder )
            ] |> Encode.object

          param getter encoder =
            Field.param ( res |> getter ) >>
            Maybe.map (Tuple.mapSecond (encode encoder))
        in
          [ fields.name  |> param get_name  Encode.string
          , fields.memo  |> param get_memo  Encode.string
          , fields.age   |> param get_age   Encode.string
          , fields.email |> param get_email Encode.string
          , fields.tel   |> param get_tel   Encode.string
          ]
          |> List.filterMap identity
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


view : HttpView.Model Response -> Form -> HttpView.Model Data.Response -> Maybe View
view put form = HttpView.response >> Maybe.map
  (\res ->
    let
      fields = form |> Edit.fields
    in
      { error       = V.conflict
      , form        = form
      , last        = res
      , isConflict  = put |> HttpView.isConflict
      , isDifferent = Data.isDifferentResponse
      }
      |> Edit.options
      |> Tuple.mapSecond
        ( Apply.apply5 (Conflict.compose5 ViewForm)
          ( ( name_,  get_name  ) |> Conflict.single [ fields.name |> V.blank ] )
          ( ( memo_,  get_memo  ) |> Conflict.single [] )
          ( ( age_,   get_age   ) |> Conflict.single [] )
          ( ( email_, get_email ) |> Conflict.single [] )
          ( ( tel_,   get_tel   ) |> Conflict.single [] )
        )
      |> Edit.view
  )
