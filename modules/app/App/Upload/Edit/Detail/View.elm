module GettoUpload.App.Upload.Edit.Detail.View exposing
  ( Form
  , View
  , Prop
  , Response
  , response
  , init
  , pairs
  , params
  , encodeForm
  , decodeForm
  , edit
  , view
  )
import GettoUpload.App.Upload.Edit.Data.View as Data
import GettoUpload.View.Http as HttpView

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

type alias ViewModel a = Validate.Model (Conflict.Form Form a)

type alias Form = Edit.Model Data.Response Fields
type alias Fields =
  { birthday : Field String
  , start_at : Field String
  , gender   : Field String
  , quality  : Field String
  , roles    : Field (Set String)
  }

type alias View =
  { isStatic : Bool
  , hasError : Bool
  , state    : HttpView.State
  , response : Maybe Data.Response
  , form :
    { birthday : Edit.State Form String
    , start_at : Edit.State Form String
    , gender   : Edit.State Form String
    , quality  : Edit.State Form String
    , roles    : Edit.State Form (Set String)
    }
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

get_birthday = .detail >> .birthday
get_start_at = .detail >> .start_at
get_gender   = .detail >> .gender
get_quality  = .detail >> .quality
get_roles    = .detail >> .roles


init : String -> Form
init signature = Edit.init
  { birthday = Field.init signature "birthday" Conflict.none ""
  , start_at = Field.init signature "start_at" Conflict.none ""
  , gender   = Field.init signature "gender"   Conflict.none ""
  , quality  = Field.init signature "quality"  Conflict.none ""
  , roles    = Field.init signature "roles"    Conflict.none Set.empty
  }

pairs : Form -> List ( String, String )
pairs = Edit.fields >>
  (\fields ->
    [ fields.birthday |> Field.pair
    , fields.start_at |> Field.pair
    ]
  )

params : HttpView.Model Data.Response -> Form -> Encode.Value
params get = Edit.fields >>
  (\fields ->
    let
      body = get |> HttpView.response |> Maybe.map HttpView.body

      set encoder = Set.toList >> Encode.list encoder

      encode encoder values =
        [ ( "from", values.from |> encoder )
        , ( "to",   values.to   |> encoder )
        ] |> Encode.object
    in
      [ ( "birthday", body |> Maybe.map get_birthday, fields.birthday ) |> Edit.filter (encode Encode.string)
      , ( "start_at", body |> Maybe.map get_start_at, fields.start_at ) |> Edit.filter (encode Encode.string)
      , ( "gender",   body |> Maybe.map get_gender,   fields.gender   ) |> Edit.filter (encode Encode.string)
      , ( "quality",  body |> Maybe.map get_quality,  fields.quality  ) |> Edit.filter (encode Encode.string)
      , ( "roles",    body |> Maybe.map get_roles,    fields.roles    ) |> Edit.filter (encode (set Encode.string))
      ]
      |> List.filterMap identity
      |> Encode.object
  )

encodeForm : Form -> Encode.Value
encodeForm = Edit.encode Data.encodeResponse encodeFields

encodeFields : Fields -> Encode.Value
encodeFields fields =
  [ ( "birthday", fields.birthday |> Field.value |> Encode.string )
  , ( "start_at", fields.start_at |> Field.value |> Encode.string )
  , ( "gender",   fields.gender   |> Field.value |> Encode.string )
  , ( "quality",  fields.quality  |> Field.value |> Encode.string )
  , ( "roles",    fields.roles    |> Field.value |> Set.toList |> Encode.list Encode.string )
  ] |> Encode.object

decodeForm : Decode.Value -> Form -> Form
decodeForm = Edit.decode Data.decodeResponse decodeFields

decodeFields : Decode.Value -> Form -> Form
decodeFields value form =
  form
  |> Form.setIf birthday_ ( value |> decode "birthday" Decode.string )
  |> Form.setIf start_at_ ( value |> decode "start_at" Decode.string )
  |> Form.setIf gender_   ( value |> decode "gender"   Decode.string )
  |> Form.setIf quality_  ( value |> decode "quality"  Decode.string )
  |> Form.setIf roles_    ( value |> decode "roles"  ((Decode.list Decode.string) |> Decode.map Set.fromList) )

decode : String -> Decode.Decoder a -> Decode.Value -> Maybe a
decode key decoder = Decode.decodeValue (Decode.at [key] decoder) >> Result.toMaybe

edit : Data.Response -> Form -> Form
edit res form =
  let
    body = res |> HttpView.body
  in
    form
    |> Form.set birthday_ (body |> get_birthday)
    |> Form.set start_at_ (body |> get_start_at)
    |> Form.set gender_   (body |> get_gender)
    |> Form.set quality_  (body |> get_quality)
    |> Form.set roles_    (body |> get_roles)


view : HttpView.Model Data.Response -> Form -> View
view http form =
  let
    error = "conflict"

    res = http |> HttpView.response

    data =
      ( form |> Edit.response |> Maybe.map HttpView.body
      , res  |> Maybe.map HttpView.body
      )

    fields = form |> Edit.fields

    model =
      { birthday = form |> Conflict.init error data ( get_birthday, ( birthday_, [] ) )
      , start_at = form |> Conflict.init error data ( get_start_at, ( start_at_, [] ) )
      , gender   = form |> Conflict.init error data ( get_gender,   ( gender_,   [] ) )
      , quality  = form |> Conflict.init error data ( get_quality,  ( quality_,  [] ) )
      , roles    = form |> Conflict.init error data ( get_roles,    ( roles_,    [] ) )
      }

    errors = List.concat
      [ model.birthday |> Validate.errors
      , model.start_at |> Validate.errors
      , model.gender   |> Validate.errors
      , model.quality  |> Validate.errors
      , model.roles    |> Validate.errors
      ]

    expose = Edit.expose Data.isDifferentResponse form res
  in
    { isStatic = form   |> Edit.isStatic
    , hasError = errors |> List.isEmpty |> not
    , state    = http   |> HttpView.state
    , response = res
    , form     =
      { birthday = model.birthday |> expose
      , start_at = model.start_at |> expose
      , gender   = model.gender   |> expose
      , quality  = model.quality  |> expose
      , roles    = model.roles    |> expose
      }
    }
