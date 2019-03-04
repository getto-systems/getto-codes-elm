module GettoUpload.App.Upload.New.Register.View exposing
  ( Form
  , View
  , Prop
  , Response
  , response
  , init
  , encodeForm
  , decodeForm
  , view
  )
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate
import Getto.Http.Header.Decode as HeaderDecode

import Json.Encode as Encode
import Json.Decode as Decode
import File exposing ( File )

import Set exposing ( Set )

type alias Attribute = ()
type alias Prop  a = Form.Prop Form Attribute a
type alias Field a = Field.Model Attribute a

type alias ViewModel a = ( String, Form.Model Form Attribute a, List String )

type alias Form =
  { name     : Field String
  , text     : Field (List File)
  , memo     : Field String
  , age      : Field String
  , email    : Field String
  , tel      : Field String
  , birthday : Field String
  , start_at : Field String
  , gender   : Field String
  , quality  : Field String
  , roles    : Field (Set String)
  }

type alias View =
  { hasError : Bool
  , form     :
    { name     : ViewModel String
    , text     : ViewModel (List File)
    , memo     : ViewModel String
    , age      : ViewModel String
    , email    : ViewModel String
    , tel      : ViewModel String
    , birthday : ViewModel String
    , start_at : ViewModel String
    , gender   : ViewModel String
    , quality  : ViewModel String
    , roles    : ViewModel (Set String)
    }
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
  { name     = Field.init signature "name"     () ""
  , text     = Field.init signature "text"     () []
  , memo     = Field.init signature "memo"     () ""
  , age      = Field.init signature "age"      () ""
  , email    = Field.init signature "email"    () ""
  , tel      = Field.init signature "tel"      () ""
  , birthday = Field.init signature "birthday" () ""
  , start_at = Field.init signature "start_at" () ""
  , gender   = Field.init signature "gender"   () ""
  , quality  = Field.init signature "quality"  () ""
  , roles    = Field.init signature "roles"    () Set.empty
  }

encodeForm : Form -> Encode.Value
encodeForm form =
  [ ( "name",     form.name     |> Field.value |> Encode.string )
  , ( "memo",     form.memo     |> Field.value |> Encode.string )
  , ( "age",      form.age      |> Field.value |> Encode.string )
  , ( "email",    form.email    |> Field.value |> Encode.string )
  , ( "tel",      form.tel      |> Field.value |> Encode.string )
  , ( "birthday", form.birthday |> Field.value |> Encode.string )
  , ( "start_at", form.start_at |> Field.value |> Encode.string )
  , ( "gender",   form.gender   |> Field.value |> Encode.string )
  , ( "quality",  form.quality  |> Field.value |> Encode.string )
  , ( "roles",    form.roles    |> Field.value |> Set.toList |> Encode.list Encode.string )
  ] |> Encode.object

decodeForm : Decode.Value -> Form -> Form
decodeForm value form =
  form
  |> Form.setIf name_     ( value |> decode "name"     Decode.string )
  |> Form.setIf memo_     ( value |> decode "memo"     Decode.string )
  |> Form.setIf age_      ( value |> decode "age"      Decode.string )
  |> Form.setIf email_    ( value |> decode "email"    Decode.string )
  |> Form.setIf tel_      ( value |> decode "tel"      Decode.string )
  |> Form.setIf birthday_ ( value |> decode "birthday" Decode.string )
  |> Form.setIf start_at_ ( value |> decode "start_at" Decode.string )
  |> Form.setIf gender_   ( value |> decode "gender"   Decode.string )
  |> Form.setIf quality_  ( value |> decode "quality"  Decode.string )
  |> Form.setIf roles_    ( value |> decode "roles"  ((Decode.list Decode.string) |> Decode.map Set.fromList) )


decode : String -> Decode.Decoder a -> Decode.Value -> Maybe a
decode name decoder = Decode.decodeValue (Decode.at [name] decoder) >> Result.toMaybe

view : Form -> View
view form =
  let
    blank  = Validate.blank "blank"
    noFile = Validate.empty "no-file"

    model =
      { name     = form |> Validate.init Validate.none ( name_,     [ form.name |> blank ] )
      , text     = form |> Validate.init Validate.none ( text_,     [ form.text |> noFile ] )
      , memo     = form |> Validate.init Validate.none ( memo_,     [] )
      , age      = form |> Validate.init Validate.none ( age_,      [] )
      , email    = form |> Validate.init Validate.none ( email_,    [] )
      , tel      = form |> Validate.init Validate.none ( tel_,      [] )
      , birthday = form |> Validate.init Validate.none ( birthday_, [] )
      , start_at = form |> Validate.init Validate.none ( start_at_, [] )
      , gender   = form |> Validate.init Validate.none ( gender_,   [] )
      , quality  = form |> Validate.init Validate.none ( quality_,  [] )
      , roles    = form |> Validate.init Validate.none ( roles_,    [] )
      }
    errors = List.concat
      [ model.name     |> Validate.errors
      , model.text     |> Validate.errors
      , model.memo     |> Validate.errors
      , model.age      |> Validate.errors
      , model.email    |> Validate.errors
      , model.tel      |> Validate.errors
      , model.birthday |> Validate.errors
      , model.start_at |> Validate.errors
      , model.gender   |> Validate.errors
      , model.quality  |> Validate.errors
      , model.roles    |> Validate.errors
      ]
  in
    { hasError = errors |> List.isEmpty |> not
    , form     =
      { name     = model.name     |> Validate.expose
      , text     = model.text     |> Validate.expose
      , memo     = model.memo     |> Validate.expose
      , age      = model.age      |> Validate.expose
      , email    = model.email    |> Validate.expose
      , tel      = model.tel      |> Validate.expose
      , birthday = model.birthday |> Validate.expose
      , start_at = model.start_at |> Validate.expose
      , gender   = model.gender   |> Validate.expose
      , quality  = model.quality  |> Validate.expose
      , roles    = model.roles    |> Validate.expose
      }
    }
