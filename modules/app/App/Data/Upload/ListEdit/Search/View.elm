module GettoUpload.App.Data.Upload.ListEdit.Search.View exposing
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
import GettoUpload.App.Data.Upload.ListEdit.Data.View as Data
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

type alias Form =
  { name   : Present.Field String
  , gender : Present.Field String
  }

type alias Prop a = Present.Prop Form a
type alias Single a = Present.Single Form {} a

type alias View =
  { name   : Single String
  , gender : Single String
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
  { name   = "name"   |> Field.init signature () ""
  , gender = "gender" |> Field.init signature () ""
  }

encodeForm : Form -> QueryEncode.Value
encodeForm form = QueryEncode.object
  [ form.name   |> Field.name_value |> Tuple.mapSecond QueryEncode.string
  , form.gender |> Field.name_value |> Tuple.mapSecond QueryEncode.string
  ]

decodeForm : List String -> QueryDecode.Value -> Form -> Form
decodeForm names value form =
  let
    qEntryAt field = value |> QueryDecode.entryAt (names ++ [form |> field |> Field.name]) QueryDecode.string
  in
    form
    |> Form.setIf name_   ( qEntryAt .name   )
    |> Form.setIf gender_ ( qEntryAt .gender )

view : Form -> View
view = Apply.map2 View
  ( name_   |> Present.single Present.string )
  ( gender_ |> Present.single Present.string )
