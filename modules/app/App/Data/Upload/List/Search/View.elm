module GettoUpload.App.Data.Upload.List.Search.View exposing
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
import Getto.Field.Present as Present
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

import Json.Encode as Encode
import Json.Decode as Decode

import Set exposing ( Set )

type alias Attribute = ()
type alias Prop  a = Form.Prop Form Attribute a
type alias Field a = Field.Model Attribute a

type alias ViewModel    a = ( String, Form.Model   Form Attribute a, Bool )
type alias BetweenModel a = ( String, Form.Between Form Attribute a, Bool )

type alias Form =
  { name          : Field String
  , age_gteq      : Field String
  , age_lteq      : Field String
  , email         : Field String
  , tel           : Field String
  , birthday_gteq : Field String
  , birthday_lteq : Field String
  , start_at_gteq : Field String
  , start_at_lteq : Field String
  , gender        : Field String
  , roles         : Field (Set String)
  }

type alias View =
  { name     : ViewModel String
  , email    : ViewModel String
  , tel      : ViewModel String
  , gender   : ViewModel String
  , roles    : ViewModel (Set String)
  , age      : BetweenModel String
  , birthday : BetweenModel String
  , start_at : BetweenModel String
  }

type alias Response = HttpView.Response ResponseHeader ResponseBody
type alias ResponseHeader =
  { max : Int
  }
type alias ResponseBody = List Upload
type alias Upload =
  { id       : Int
  , name     : String
  , gender   : String
  , roles    : List String
  , comments : List Comment
  }
type alias Comment =
  { user : String
  , text : String
  , likes : List Like
  }
type alias Like =
  { user : String
  , text : String
  }


response : HttpView.ResponseDecoder Response
response = HttpView.decoder
  { header = HeaderDecode.map ResponseHeader
    ( HeaderDecode.at "x-paging-max" HeaderDecode.int )
  , body = Decode.list
    ( Decode.map5 Upload
      ( Decode.at ["id"]        Decode.int )
      ( Decode.at ["name"]      Decode.string )
      ( Decode.at ["gender"]    Decode.string )
      ( Decode.at ["roles"]    (Decode.list Decode.string) )
      ( Decode.at ["comments"] (Decode.list (Decode.map3 Comment
        ( Decode.at ["user"]   Decode.string )
        ( Decode.at ["text"]   Decode.string )
        ( Decode.at ["likes"] (Decode.list (Decode.map2 Like
          ( Decode.at ["user"]   Decode.string )
          ( Decode.at ["text"]   Decode.string )
        )) )
      )) )
    )
  }


name_          = Form.prop .name          (\v m -> { m | name          = v })
age_gteq_      = Form.prop .age_gteq      (\v m -> { m | age_gteq      = v })
age_lteq_      = Form.prop .age_lteq      (\v m -> { m | age_lteq      = v })
email_         = Form.prop .email         (\v m -> { m | email         = v })
tel_           = Form.prop .tel           (\v m -> { m | tel           = v })
birthday_gteq_ = Form.prop .birthday_gteq (\v m -> { m | birthday_gteq = v })
birthday_lteq_ = Form.prop .birthday_lteq (\v m -> { m | birthday_lteq = v })
start_at_gteq_ = Form.prop .start_at_gteq (\v m -> { m | start_at_gteq = v })
start_at_lteq_ = Form.prop .start_at_lteq (\v m -> { m | start_at_lteq = v })
gender_        = Form.prop .gender        (\v m -> { m | gender        = v })
roles_         = Form.prop .roles         (\v m -> { m | roles         = v })

init : String -> Form
init signature =
  { name          = Field.init signature "name"          () ""
  , age_gteq      = Field.init signature "age_gteq"      () ""
  , age_lteq      = Field.init signature "age_lteq"      () ""
  , email         = Field.init signature "email"         () ""
  , tel           = Field.init signature "tel"           () ""
  , birthday_gteq = Field.init signature "birthday_gteq" () ""
  , birthday_lteq = Field.init signature "birthday_lteq" () ""
  , start_at_gteq = Field.init signature "start_at_gteq" () ""
  , start_at_lteq = Field.init signature "start_at_lteq" () ""
  , gender        = Field.init signature "gender"        () ""
  , roles         = Field.init signature "roles"         () Set.empty
  }

encodeForm : Form -> QueryEncode.Value
encodeForm form = QueryEncode.object
  [ ( "name",           form.name          |> Field.value |> QueryEncode.string )
  , ( "age_gteq",       form.age_gteq      |> Field.value |> QueryEncode.string )
  , ( "age_lteq",       form.age_lteq      |> Field.value |> QueryEncode.string )
  , ( "email",          form.email         |> Field.value |> QueryEncode.string )
  , ( "tel",            form.tel           |> Field.value |> QueryEncode.string )
  , ( "birthday_gteq",  form.birthday_gteq |> Field.value |> QueryEncode.string )
  , ( "birthday_lteq",  form.birthday_lteq |> Field.value |> QueryEncode.string )
  , ( "start_at_gteq",  form.start_at_gteq |> Field.value |> QueryEncode.string )
  , ( "start_at_lteq",  form.start_at_lteq |> Field.value |> QueryEncode.string )
  , ( "gender",         form.gender        |> Field.value |> QueryEncode.string )
  , ( "roles",          form.roles         |> Field.value |> QueryEncode.set QueryEncode.string )
  ]

decodeForm : List String -> QueryDecode.Value -> Form -> Form
decodeForm names value form =
  let
    qEntryAt name = QueryDecode.entryAt (names ++ [name]) QueryDecode.string
    qListAt  name = QueryDecode.listAt  (names ++ [name]) QueryDecode.string
  in
    form
    |> Form.setIf name_          ( value |> qEntryAt "name"          )
    |> Form.setIf age_gteq_      ( value |> qEntryAt "age_gteq"      )
    |> Form.setIf age_lteq_      ( value |> qEntryAt "age_lteq"      )
    |> Form.setIf email_         ( value |> qEntryAt "email"         )
    |> Form.setIf tel_           ( value |> qEntryAt "tel"           )
    |> Form.setIf birthday_gteq_ ( value |> qEntryAt "birthday_gteq" )
    |> Form.setIf birthday_lteq_ ( value |> qEntryAt "birthday_lteq" )
    |> Form.setIf start_at_gteq_ ( value |> qEntryAt "start_at_gteq" )
    |> Form.setIf start_at_lteq_ ( value |> qEntryAt "start_at_lteq" )
    |> Form.setIf gender_        ( value |> qEntryAt "gender"        )
    |> Form.setIf roles_         ( value |> qListAt  "roles" |> Maybe.map Set.fromList )

view : Form -> View
view form =
  let
    model =
      { name          = ( name_,          Present.string )
      , age_gteq      = ( age_gteq_,      Present.string )
      , age_lteq      = ( age_lteq_,      Present.string )
      , email         = ( email_,         Present.string )
      , tel           = ( tel_,           Present.string )
      , birthday_gteq = ( birthday_gteq_, Present.string )
      , birthday_lteq = ( birthday_lteq_, Present.string )
      , start_at_gteq = ( start_at_gteq_, Present.string )
      , start_at_lteq = ( start_at_lteq_, Present.string )
      , gender        = ( gender_,        Present.string )
      , roles         = ( roles_,         Present.set )
      }
  in
    { name     = form |> Present.init model.name   |> Present.expose
    , email    = form |> Present.init model.email  |> Present.expose
    , tel      = form |> Present.init model.tel    |> Present.expose
    , gender   = form |> Present.init model.gender |> Present.expose
    , roles    = form |> Present.init model.roles  |> Present.expose
    , age      = form |> Present.between "age"      { gteq = model.age_gteq,      lteq = model.age_lteq }
                 |> Present.expose
    , birthday = form |> Present.between "birthday" { gteq = model.birthday_gteq, lteq = model.birthday_lteq }
                 |> Present.expose
    , start_at = form |> Present.between "start_at" { gteq = model.start_at_gteq, lteq = model.start_at_lteq }
                 |> Present.expose
    }
