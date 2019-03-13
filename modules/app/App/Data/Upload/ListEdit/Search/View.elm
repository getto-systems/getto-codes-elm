module GettoUpload.App.Data.Upload.ListEdit.Search.View exposing
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
import GettoUpload.App.Data.Upload.ListEdit.Data.View as Data
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Present as Present
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

import Json.Encode as Encode
import Json.Decode as Decode

type alias Attribute = ()
type alias Prop  a = Form.Prop Form Attribute a
type alias Field a = Field.Model Attribute a

type alias ViewModel a = ( String, Form.Model Form Attribute a, Bool )

type alias Form =
  { name   : Field String
  , gender : Field String
  }

type alias View =
  { name   : ViewModel String
  , gender : ViewModel String
  }

type alias Response = HttpView.Response ResponseHeader ResponseBody
type alias ResponseHeader =
  { max : Int
  }
type alias ResponseBody = List Data.Upload


response : HttpView.ResponseDecoder Response
response = HttpView.decoder
  { header = HeaderDecode.map ResponseHeader
    ( HeaderDecode.at "x-paging-max" HeaderDecode.int )
  , body = Decode.list Data.upload
  }


name_   = Form.prop .name   (\v m -> { m | name   = v })
gender_ = Form.prop .gender (\v m -> { m | gender = v })

init : String -> Form
init signature =
  { name   = Field.init signature "name"   () ""
  , gender = Field.init signature "gender" () ""
  }

encodeForm : Form -> QueryEncode.Value
encodeForm form = QueryEncode.object
  [ ( "name",   form.name   |> Field.value |> QueryEncode.string )
  , ( "gender", form.gender |> Field.value |> QueryEncode.string )
  ]

decodeForm : List String -> QueryDecode.Value -> Form -> Form
decodeForm names value form =
  let
    qEntryAt name = QueryDecode.entryAt (names ++ [name]) QueryDecode.string
  in
    form
    |> Form.setIf name_   ( value |> qEntryAt "name"   )
    |> Form.setIf gender_ ( value |> qEntryAt "gender" )

view : Form -> View
view form =
  let
    model =
      { name   = ( name_,   Present.string )
      , gender = ( gender_, Present.string )
      }
  in
    { name   = form |> Present.init model.name   |> Present.expose
    , gender = form |> Present.init model.gender |> Present.expose
    }
