module GettoUpload.App.Data.Upload.ListEdit.Info.View exposing
  ( Units
  , Form
  , Fields
  , View
  , Prop
  , Response
  , response
  , rows
  , empty
  , get
  , update
  , updateForm
  , clear
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

import Dict exposing ( Dict )

type Units = Units String (Dict Int Unit)
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

type alias View = Data.Upload ->
  ( Data.Upload
  , { isStatic : Bool
    , state    : Edit.AggregateState
    , get      : HttpView.Model Data.Response
    , put      : HttpView.Model Response
    , form :
      { name   : Edit.State Form String
      , gender : Edit.State Form String
      }
    }
  )

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


rows : Units -> List Data.Upload
rows (Units _ dict) = dict |> Dict.values |> List.map .row


empty : String -> Units
empty signature = Units signature Dict.empty

get : Data.Upload -> Units -> Unit
get row (Units signature dict) = dict |> Dict.get row.id |> Maybe.withDefault (init signature row)

update : Data.Upload -> (Unit -> Unit) -> Units -> Units
update row f (Units signature dict) = Units signature
  ( dict |> Dict.update row.id (Maybe.withDefault (init signature row) >> f >> Just) )

updateForm : Data.Upload -> (Form -> Form) -> Units -> Units
updateForm row f = update row (\unit -> { unit | form = unit.form |> f })

clear : Units -> Units
clear (Units signature _) = empty signature


init : String -> Data.Upload -> Unit
init signature row =
  { get  = HttpView.empty
  , put  = HttpView.empty
  , form = signature |> withId row.id |> initForm
  , row  = row
  }

withId : Int -> String -> String
withId id signature = signature ++ "-" ++ (id |> String.fromInt)

initForm : String -> Form
initForm signature =
  { name   = "name"   |> Field.init signature Conflict.none ""
  , gender = "gender" |> Field.init signature Conflict.none ""
  }
  |> Edit.init

params : Data.Upload -> Units -> Encode.Value
params row = get row >> .form >> Edit.fields >>
  (\fields ->
    let
      res = row |> Data.toResponse

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
      [ fields.name   |> map_name_param get_name   Encode.string
      , fields.gender |> map_name_param get_gender Encode.string
      ]
      |> List.filterMap (\(k,v) -> v |> Maybe.map (\val -> ( k, val )))
      |> Encode.object
  )

edit : Data.Response -> Form -> Form
edit res form =
  form
  |> Form.set name_   (res |> get_name)
  |> Form.set gender_ (res |> get_gender)


view : Units -> View
view units row = units |> get row |>
  (\unit ->
    let
      res = unit.get |> HttpView.response |> Maybe.withDefault (row |> Data.toResponse)

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
      ( row
      , { isStatic = isStatic
        , state    = errors |> Edit.aggregate modifieds
        , put = unit.put
        , get = unit.get
        , form     =
          { name   = model.name   |> Edit.expose isStatic
          , gender = model.gender |> Edit.expose isStatic
          }
        }
      )
  )