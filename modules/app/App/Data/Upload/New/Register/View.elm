module GettoCodes.App.Data.Upload.New.Register.View exposing
  ( Form
  , Prop
  , View
  , Response
  , response
  , init
  , params
  , encodeForm
  , decodeForm
  , view
  )
import GettoCodes.View.Http as HttpView
import GettoCodes.View.Validate as V

import Getto.Apply as Apply
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Http.Part as Part

import Json.Encode as Encode
import Json.Decode as Decode
import File exposing ( File )

import Set exposing ( Set )

type alias Form =
  { name     : Validate.Field String
  , text     : Validate.Field (List File)
  , memo     : Validate.Field String
  , age      : Validate.Field String
  , email    : Validate.Field String
  , tel      : Validate.Field String
  , birthday : Validate.Field String
  , start_at : Validate.Field String
  , gender   : Validate.Field String
  , quality  : Validate.Field String
  , roles    : Validate.Field (Set String)
  }

type alias Prop a = Validate.Prop Form a
type alias Single a = Validate.Single Form {} a

type alias View = Validate.View ViewForm
type alias ViewForm =
  { name     : Single String
  , text     : Single (List File)
  , memo     : Single String
  , age      : Single String
  , email    : Single String
  , tel      : Single String
  , birthday : Single String
  , start_at : Single String
  , gender   : Single String
  , quality  : Single String
  , roles    : Single (Set String)
  }

type alias Response = HttpView.Response ResponseHeader ResponseBody
type alias ResponseHeader =
  { id : Int
  }
type alias ResponseBody = ()


response : HttpView.ResponseDecoder Response
response = HttpView.decoder
  { header = HeaderDecode.map ResponseHeader
    ( HeaderDecode.at "x-upload-id" HeaderDecode.int )
  , body = Decode.succeed ()
  }


name_     = Form.prop .name     (\v m -> { m | name     = v })
text_     = Form.prop .text     (\v m -> { m | text     = v })
memo_     = Form.prop .memo     (\v m -> { m | memo     = v })
age_      = Form.prop .age      (\v m -> { m | age      = v })
email_    = Form.prop .email    (\v m -> { m | email    = v })
tel_      = Form.prop .tel      (\v m -> { m | tel      = v })
birthday_ = Form.prop .birthday (\v m -> { m | birthday = v })
start_at_ = Form.prop .start_at (\v m -> { m | start_at = v })
gender_   = Form.prop .gender   (\v m -> { m | gender   = v })
quality_  = Form.prop .quality  (\v m -> { m | quality  = v })
roles_    = Form.prop .roles    (\v m -> { m | roles    = v })

init : String -> Form
init signature =
  { name     = "name"     |> Validate.init signature ""
  , text     = "text"     |> Validate.init signature []
  , memo     = "memo"     |> Validate.init signature ""
  , age      = "age"      |> Validate.init signature ""
  , email    = "email"    |> Validate.init signature ""
  , tel      = "tel"      |> Validate.init signature ""
  , birthday = "birthday" |> Validate.init signature ""
  , start_at = "start_at" |> Validate.init signature ""
  , gender   = "gender"   |> Validate.init signature ""
  , quality  = "quality"  |> Validate.init signature ""
  , roles    = "roles"    |> Validate.init signature Set.empty
  }

params : Form -> Part.Value
params form = Part.object
  [ form.name     |> Field.name_value |> Tuple.mapSecond  Part.string
  , form.text     |> Field.name_value |> Tuple.mapSecond (Part.list Part.file)
  , form.memo     |> Field.name_value |> Tuple.mapSecond  Part.string
  , form.age      |> Field.name_value |> Tuple.mapSecond  Part.string
  , form.email    |> Field.name_value |> Tuple.mapSecond  Part.string
  , form.tel      |> Field.name_value |> Tuple.mapSecond  Part.string
  , form.birthday |> Field.name_value |> Tuple.mapSecond  Part.string
  , form.start_at |> Field.name_value |> Tuple.mapSecond  Part.string
  , form.gender   |> Field.name_value |> Tuple.mapSecond  Part.string
  , form.quality  |> Field.name_value |> Tuple.mapSecond  Part.string
  , form.roles    |> Field.name_value |> Tuple.mapSecond (Set.toList >> Part.list Part.string)
  ]

encodeForm : Form -> Encode.Value
encodeForm form =
  [ form.name     |> Field.name_value |> Tuple.mapSecond  Encode.string
  , form.memo     |> Field.name_value |> Tuple.mapSecond  Encode.string
  , form.age      |> Field.name_value |> Tuple.mapSecond  Encode.string
  , form.email    |> Field.name_value |> Tuple.mapSecond  Encode.string
  , form.tel      |> Field.name_value |> Tuple.mapSecond  Encode.string
  , form.birthday |> Field.name_value |> Tuple.mapSecond  Encode.string
  , form.start_at |> Field.name_value |> Tuple.mapSecond  Encode.string
  , form.gender   |> Field.name_value |> Tuple.mapSecond  Encode.string
  , form.quality  |> Field.name_value |> Tuple.mapSecond  Encode.string
  , form.roles    |> Field.name_value |> Tuple.mapSecond (Set.toList >> Encode.list Encode.string)
  ] |> Encode.object

decodeForm : Decode.Value -> Form -> Form
decodeForm value form =
  let
    decode field decoder =
      value
      |> Decode.decodeValue
        (Decode.at [form |> field |> Field.name] decoder)
      |> Result.toMaybe

    set = Decode.list Decode.string |> Decode.map Set.fromList
  in
    form
    |> Form.setIf name_     ( decode .name     Decode.string )
    |> Form.setIf memo_     ( decode .memo     Decode.string )
    |> Form.setIf age_      ( decode .age      Decode.string )
    |> Form.setIf email_    ( decode .email    Decode.string )
    |> Form.setIf tel_      ( decode .tel      Decode.string )
    |> Form.setIf birthday_ ( decode .birthday Decode.string )
    |> Form.setIf start_at_ ( decode .start_at Decode.string )
    |> Form.setIf gender_   ( decode .gender   Decode.string )
    |> Form.setIf quality_  ( decode .quality  Decode.string )
    |> Form.setIf roles_    ( decode .roles    set )

view : Form -> View
view form = form |> Apply.map11 (Validate.compose11 ViewForm)
  ( name_     |> Validate.single [ form.name |> V.blank ] )
  ( text_     |> Validate.single [ form.text |> V.noFile ] )
  ( memo_     |> Validate.single [] )
  ( age_      |> Validate.single [] )
  ( email_    |> Validate.single [] )
  ( tel_      |> Validate.single [] )
  ( birthday_ |> Validate.single [] )
  ( start_at_ |> Validate.single [] )
  ( gender_   |> Validate.single [] )
  ( quality_  |> Validate.single [] )
  ( roles_    |> Validate.single [] )
