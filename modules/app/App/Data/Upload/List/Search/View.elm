module GettoUpload.App.Data.Upload.List.Search.View exposing
  ( Form
  , Prop
  , View
  , Response
  , response
  , init
  , encodeForm
  , decodeForm
  , view
  )
import GettoUpload.View.Http as HttpView

import Getto.Apply as Apply
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Present as Present
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

import Json.Encode as Encode
import Json.Decode as Decode

import Set exposing ( Set )

type alias Form =
  { name          : Present.Field String
  , age_gteq      : Present.Field String
  , age_lteq      : Present.Field String
  , email         : Present.Field String
  , tel           : Present.Field String
  , birthday_gteq : Present.Field String
  , birthday_lteq : Present.Field String
  , start_at_gteq : Present.Field String
  , start_at_lteq : Present.Field String
  , gender        : Present.Field String
  , roles         : Present.Field (Set String)
  }

type alias Prop a = Present.Prop Form a
type alias Single  a = Present.Single  Form {} a
type alias Between a = Present.Between Form {} a

type alias View =
  { name     : Single String
  , email    : Single String
  , tel      : Single String
  , gender   : Single String
  , roles    : Single (Set String)
  , age      : Between String
  , birthday : Between String
  , start_at : Between String
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
  { name          = "name"          |> Present.init signature ""
  , age_gteq      = "age_gteq"      |> Present.init signature ""
  , age_lteq      = "age_lteq"      |> Present.init signature ""
  , email         = "email"         |> Present.init signature ""
  , tel           = "tel"           |> Present.init signature ""
  , birthday_gteq = "birthday_gteq" |> Present.init signature ""
  , birthday_lteq = "birthday_lteq" |> Present.init signature ""
  , start_at_gteq = "start_at_gteq" |> Present.init signature ""
  , start_at_lteq = "start_at_lteq" |> Present.init signature ""
  , gender        = "gender"        |> Present.init signature ""
  , roles         = "roles"         |> Present.init signature Set.empty
  }

encodeForm : Form -> QueryEncode.Value
encodeForm form = QueryEncode.object
  [ ( form.name          |> Field.name_value |> Tuple.mapSecond  QueryEncode.string )
  , ( form.age_gteq      |> Field.name_value |> Tuple.mapSecond  QueryEncode.string )
  , ( form.age_lteq      |> Field.name_value |> Tuple.mapSecond  QueryEncode.string )
  , ( form.email         |> Field.name_value |> Tuple.mapSecond  QueryEncode.string )
  , ( form.tel           |> Field.name_value |> Tuple.mapSecond  QueryEncode.string )
  , ( form.birthday_gteq |> Field.name_value |> Tuple.mapSecond  QueryEncode.string )
  , ( form.birthday_lteq |> Field.name_value |> Tuple.mapSecond  QueryEncode.string )
  , ( form.start_at_gteq |> Field.name_value |> Tuple.mapSecond  QueryEncode.string )
  , ( form.start_at_lteq |> Field.name_value |> Tuple.mapSecond  QueryEncode.string )
  , ( form.gender        |> Field.name_value |> Tuple.mapSecond  QueryEncode.string )
  , ( form.roles         |> Field.name_value |> Tuple.mapSecond (QueryEncode.set QueryEncode.string) )
  ]

decodeForm : List String -> QueryDecode.Value -> Form -> Form
decodeForm names value form =
  let
    qEntryAt field = value |> QueryDecode.entryAt (names ++ [form |> field |> Field.name]) QueryDecode.string
    qListAt  field = value |> QueryDecode.listAt  (names ++ [form |> field |> Field.name]) QueryDecode.string
  in
    form
    |> Form.setIf name_          ( qEntryAt .name )
    |> Form.setIf age_gteq_      ( qEntryAt .age_gteq )
    |> Form.setIf age_lteq_      ( qEntryAt .age_lteq )
    |> Form.setIf email_         ( qEntryAt .email )
    |> Form.setIf tel_           ( qEntryAt .tel )
    |> Form.setIf birthday_gteq_ ( qEntryAt .birthday_gteq )
    |> Form.setIf birthday_lteq_ ( qEntryAt .birthday_lteq )
    |> Form.setIf start_at_gteq_ ( qEntryAt .start_at_gteq )
    |> Form.setIf start_at_lteq_ ( qEntryAt .start_at_lteq )
    |> Form.setIf gender_        ( qEntryAt .gender )
    |> Form.setIf roles_         ( qListAt  .roles |> Maybe.map Set.fromList )

view : Form -> View
view = Apply.apply8 View
  ( name_   |> Present.single Present.string )
  ( email_  |> Present.single Present.string )
  ( tel_    |> Present.single Present.string )
  ( gender_ |> Present.single Present.string )
  ( roles_  |> Present.single Present.set )
  ( "age" |> Present.between
    { gteq = age_gteq_ |> Present.single Present.string
    , lteq = age_lteq_ |> Present.single Present.string
    }
  )
  ( "birthday" |> Present.between
    { gteq = birthday_gteq_ |> Present.single Present.string
    , lteq = birthday_lteq_ |> Present.single Present.string
    }
  )
  ( "start_at" |> Present.between
    { gteq = start_at_gteq_ |> Present.single Present.string
    , lteq = start_at_lteq_ |> Present.single Present.string
    }
  )
