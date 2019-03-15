module GettoUpload.App.Data.Upload.ListEdit.Detail.View exposing
  ( Unit
  , Form
  , Fields
  , View
  , Prop
  , Response
  , response
  , init
  , updateForm
  , params
  , edit
  , view
  )
import GettoUpload.App.Data.Upload.ListEdit.Data.View as Data
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Edit as Edit
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict
import Getto.Http.Header.Decode as HeaderDecode

import Json.Encode as Encode
import Json.Decode as Decode

type alias Unit =
  { get  : HttpView.Model Data.Response
  , put  : HttpView.Model Response
  , form : Form
  , row  : Data.Upload
  }

type alias Prop  a = Conflict.Prop Form a
type alias Field a = Conflict.Field a

type alias Form = Edit.Model Data.Response Fields
type alias Fields =
  { name   : Field String
  , gender : Field String
  }

type alias View = Maybe
  { state : Edit.AggregateState
  , get   : HttpView.Model Data.Response
  , put   : HttpView.Model Response
  , form :
    { name   : Edit.State Form String
    , gender : Edit.State Form String
    }
  }

type alias Response = HttpView.Response () ()


response : HttpView.ResponseDecoder Response
response = HttpView.decoder
  { header = HeaderDecode.succeed ()
  , body   = Decode.succeed ()
  }


name_   = Form.prop (Edit.fields >> .name)   (\v -> Edit.update (\m -> { m | name   = v }) )
gender_ = Form.prop (Edit.fields >> .gender) (\v -> Edit.update (\m -> { m | gender = v }) )

get_name   = HttpView.body >> .name
get_gender = HttpView.body >> .gender


init : String -> Data.Upload -> (Form -> Form)-> Maybe Unit
init signature row f = Just
  { get  = HttpView.empty
  , put  = HttpView.empty
  , form = signature |> withId row.id |> initForm |> f
  , row  = row
  }

updateForm : (Form -> Form) -> Maybe Unit -> Maybe Unit
updateForm f = Maybe.map (\unit -> { unit | form = unit.form |> f })

withId : Int -> String -> String
withId id signature = signature ++ "-" ++ (id |> String.fromInt)

initForm : String -> Form
initForm signature =
  { name   = "name"   |> Field.init signature Conflict.none ""
  , gender = "gender" |> Field.init signature Conflict.none ""
  }
  |> Edit.init

params : Unit -> Encode.Value
params unit = unit.form |> Edit.fields |>
  (\fields ->
    let
      res = unit.row |> Data.toResponse

      encode encoder values =
        [ ( "from", values.from |> encoder )
        , ( "to",   values.to   |> encoder )
        ] |> Encode.object
    in
      [ ( "name",   res |> get_name   |> Just, fields.name   ) |> Edit.filter (encode Encode.string)
      , ( "gender", res |> get_gender |> Just, fields.gender ) |> Edit.filter (encode Encode.string)
      ]
      |> List.filterMap identity
      |> Encode.object
  )

edit : Data.Response -> Form -> Form
edit res form =
  form
  |> Form.set name_   (res |> get_name)
  |> Form.set gender_ (res |> get_gender)


view : Maybe Unit -> View
view = Maybe.map
  (\unit ->
    let
      res = unit.get |> HttpView.response |> Maybe.withDefault (unit.row |> Data.toResponse)

      error = "conflict"

      blank = Validate.blank "blank"

      data =
        ( unit.form |> Edit.response
        , res
        )

      fields = unit.form |> Edit.fields

      model =
        { name   = unit.form |> Conflict.init error data ( get_name,   ( name_,   [ fields.name |> blank ] ) )
        , gender = unit.form |> Conflict.init error data ( get_gender, ( gender_, [] ) )
        }

      modifieds =
        [ model.name   |> Conflict.modified
        , model.gender |> Conflict.modified
        ]

      errors = List.concat
        [ model.name   |> Conflict.form |> Validate.errors
        , model.gender |> Conflict.form |> Validate.errors
        ]

      isStatic = unit.form |> Edit.isStatic Data.isDifferentResponse res
    in
      { state = errors |> Edit.aggregate modifieds
      , put = unit.put
      , get = unit.get
      , form     =
        { name   = model.name   |> Edit.expose isStatic
        , gender = model.gender |> Edit.expose isStatic
        }
      }
  )
