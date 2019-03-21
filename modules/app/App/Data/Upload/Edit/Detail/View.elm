module GettoUpload.App.Data.Upload.Edit.Detail.View exposing
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

import Getto.Apply as Apply
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Edit as Edit
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict
import Getto.Http.Header.Decode as HeaderDecode

import Json.Encode as Encode
import Json.Decode as Decode

import Set exposing ( Set )

type alias Prop  a = Conflict.Prop Form a
type alias Field a = Conflict.Field a
type alias Single a = Conflict.Single Form {} a

type alias Form = Edit.Form Data.Response Fields
type alias Fields =
  { birthday : Field String
  , start_at : Field String
  , gender   : Field String
  , quality  : Field String
  , roles    : Field (Set String)
  }

type alias View = Edit.View ViewForm
type alias ViewForm =
  { birthday : Single String
  , start_at : Single String
  , gender   : Single String
  , quality  : Single String
  , roles    : Single (Set String)
  }

type alias Response = HttpView.Response () ()


response : HttpView.ResponseDecoder Response
response = HttpView.decoder
  { header = HeaderDecode.succeed ()
  , body   = Decode.succeed ()
  }


birthday_ = Form.prop (Edit.fields >> .birthday) (\v -> Edit.update (\m -> { m | birthday = v }) )
start_at_ = Form.prop (Edit.fields >> .start_at) (\v -> Edit.update (\m -> { m | start_at = v }) )
gender_   = Form.prop (Edit.fields >> .gender)   (\v -> Edit.update (\m -> { m | gender   = v }) )
quality_  = Form.prop (Edit.fields >> .quality)  (\v -> Edit.update (\m -> { m | quality  = v }) )
roles_    = Form.prop (Edit.fields >> .roles)    (\v -> Edit.update (\m -> { m | roles    = v }) )

get_birthday = HttpView.body >> .detail >> .birthday
get_start_at = HttpView.body >> .detail >> .start_at
get_gender   = HttpView.body >> .detail >> .gender
get_quality  = HttpView.body >> .detail >> .quality
get_roles    = HttpView.body >> .detail >> .roles


init : String -> Form
init signature = Edit.form
  { birthday = "birthday" |> Conflict.init signature ""
  , start_at = "start_at" |> Conflict.init signature ""
  , gender   = "gender"   |> Conflict.init signature ""
  , quality  = "quality"  |> Conflict.init signature ""
  , roles    = "roles"    |> Conflict.init signature Set.empty
  }

params : HttpView.Model Data.Response -> Form -> Encode.Value
params get =
  case get |> HttpView.response of
    Nothing  -> always Encode.null
    Just res -> Edit.fields >>
      (\fields ->
        let
          set encoder = Set.toList >> Encode.list encoder

          encode encoder param =
            [ ( "from", param.from |> encoder )
            , ( "to",   param.to   |> encoder )
            ] |> Encode.object

          map_name_param getter encoder =
            Field.name_value >>
            Tuple.mapSecond
              ( Edit.param ( res |> getter )
                >> Maybe.map (encode encoder)
              )
        in
          [ fields.birthday |> map_name_param get_birthday Encode.string
          , fields.start_at |> map_name_param get_start_at Encode.string
          , fields.gender   |> map_name_param get_gender   Encode.string
          , fields.quality  |> map_name_param get_quality  Encode.string
          , fields.roles    |> map_name_param get_roles   (set Encode.string)
          ]
          |> List.filterMap (\(k,v) -> v |> Maybe.map (\val -> ( k, val )))
          |> Encode.object
      )

encodeForm : Form -> Encode.Value
encodeForm = Edit.encode Data.encodeResponse encodeFields

encodeFields : Fields -> Encode.Value
encodeFields fields =
  [ fields.birthday |> Field.name_value |> Tuple.mapSecond Encode.string
  , fields.start_at |> Field.name_value |> Tuple.mapSecond Encode.string
  , fields.gender   |> Field.name_value |> Tuple.mapSecond Encode.string
  , fields.quality  |> Field.name_value |> Tuple.mapSecond Encode.string
  , fields.roles    |> Field.name_value |> Tuple.mapSecond (Set.toList >> Encode.list Encode.string)
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

    set = Decode.list Decode.string |> Decode.map Set.fromList
  in
    form
    |> Form.setIf birthday_ ( decode .birthday Decode.string )
    |> Form.setIf start_at_ ( decode .start_at Decode.string )
    |> Form.setIf gender_   ( decode .gender   Decode.string )
    |> Form.setIf quality_  ( decode .quality  Decode.string )
    |> Form.setIf roles_    ( decode .roles    set )

edit : Data.Response -> Form -> Form
edit res form =
  form
  |> Form.set birthday_ (res |> get_birthday)
  |> Form.set start_at_ (res |> get_start_at)
  |> Form.set gender_   (res |> get_gender)
  |> Form.set quality_  (res |> get_quality)
  |> Form.set roles_    (res |> get_roles)


view : HttpView.Model Response -> Form -> HttpView.Model Data.Response -> Maybe View
view put form = HttpView.response >> Maybe.map
  (\res ->
    let
      fields = form |> Edit.fields
    in
      { error       = "conflict"
      , form        = form
      , last        = res
      , isConflict  = put |> HttpView.isConflict
      , isDifferent = Data.isDifferentResponse
      }
      |> Edit.options
      |> Tuple.mapSecond
        ( Apply.apply5 (Conflict.compose5 ViewForm)
          ( ( birthday_, get_birthday ) |> Conflict.single [] )
          ( ( start_at_, get_start_at ) |> Conflict.single [] )
          ( ( gender_,   get_gender   ) |> Conflict.single [] )
          ( ( quality_,  get_quality  ) |> Conflict.single [] )
          ( ( roles_,    get_roles    ) |> Conflict.single [] )
        )
      |> Edit.view
  )
