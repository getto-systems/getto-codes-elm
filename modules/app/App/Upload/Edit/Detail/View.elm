module GettoUpload.App.Upload.Edit.Detail.View exposing
  ( Form
  , View
  , Prop
  , State(..)
  , Response
  , response
  , init
  , params
  , encodeForm
  , decodeForm
  , toStatic
  , toEdit
  , toCommit
  , changed
  , put
  , view
  )
import GettoUpload.App.Upload.Edit.Data.View as Data
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict
import Getto.Http.Header.Decode as HeaderDecode

import Json.Encode as Encode
import Json.Decode as Decode
import File exposing ( File )

import Set exposing ( Set )

type alias Prop      a = Conflict.Prop Form a
type alias Field     a = Conflict.Field a
type alias ViewModel a = Validate.Model (Conflict.Form Form a)

type alias Form =
  { state    : EditState
  , birthday : Field String
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
    { birthday : State String
    , start_at : State String
    , gender   : State String
    , quality  : State String
    , roles    : State (Set String)
    }
  }

type alias Response = HttpView.Response () ()

type EditState
  = StaticState
  | EditState Bool Data.Response

type State a
  = Static String
  | Edit   String (Conflict.Form Form a) (Conflict.State a) (List String)

type alias ParamValue a =
  { from : a
  , to   : a
  }


response : HttpView.ResponseDecoder Response
response = HttpView.decoder
  { header = HeaderDecode.succeed ()
  , body   = Decode.succeed ()
  }


birthday_ = Form.prop .birthday (\v m -> { m | birthday = v })
start_at_ = Form.prop .start_at (\v m -> { m | start_at = v })
gender_   = Form.prop .gender   (\v m -> { m | gender   = v })
quality_  = Form.prop .quality  (\v m -> { m | quality  = v })
roles_    = Form.prop .roles    (\v m -> { m | roles    = v })

get_birthday = .detail >> .birthday
get_start_at = .detail >> .start_at
get_gender   = .detail >> .gender
get_quality  = .detail >> .quality
get_roles    = .detail >> .roles


init : String -> Form
init signature =
  { state    = StaticState
  , birthday = Field.init signature "birthday" Conflict.none ""
  , start_at = Field.init signature "start_at" Conflict.none ""
  , gender   = Field.init signature "gender"   Conflict.none ""
  , quality  = Field.init signature "quality"  Conflict.none ""
  , roles    = Field.init signature "roles"    Conflict.none Set.empty
  }

params : HttpView.Model Data.Response -> Form -> Encode.Value
params get form =
  let
    body = get |> HttpView.response |> Maybe.map HttpView.body

    set encoder = Set.toList >> Encode.list encoder

    encode encoder values =
      [ ( "from", values.from |> encoder )
      , ( "to",   values.to   |> encoder )
      ] |> Encode.object
  in
    [ ( "birthday", body |> Maybe.map get_birthday, form.birthday ) |> filter (encode Encode.string)
    , ( "start_at", body |> Maybe.map get_start_at, form.start_at ) |> filter (encode Encode.string)
    , ( "gender",   body |> Maybe.map get_gender,   form.gender   ) |> filter (encode Encode.string)
    , ( "quality",  body |> Maybe.map get_quality,  form.quality  ) |> filter (encode Encode.string)
    , ( "roles",    body |> Maybe.map get_roles,    form.roles    ) |> filter (encode (set Encode.string))
    ]
    |> List.filterMap identity
    |> Encode.object

filter : (ParamValue a -> Encode.Value) -> ( String, Maybe a, Field a ) -> Maybe ( String, Encode.Value )
filter encoder (fieldName,value,field) =
  let
    formValue = field |> Field.value

    isSame val =
      if val == formValue
        then Nothing
        else Just val
  in
    value |> Maybe.andThen isSame |> Maybe.map
      (\val ->
        ( fieldName, { from = val, to = formValue } |> encoder )
      )

encodeForm : Form -> Encode.Value
encodeForm form =
  case form.state of
    StaticState -> [ ( "state", "static" |> Encode.string ) ] |> Encode.object
    EditState _ res ->
      [ ( "state", "edit" |> Encode.string )
      , ( "response", res  |> Data.encodeResponse )
      , ( "form",     form |> encodeFields )
      ] |> Encode.object

encodeFields : Form -> Encode.Value
encodeFields form =
  [ ( "birthday", form.birthday |> Field.value |> Encode.string )
  , ( "start_at", form.start_at |> Field.value |> Encode.string )
  , ( "gender",   form.gender   |> Field.value |> Encode.string )
  , ( "quality",  form.quality  |> Field.value |> Encode.string )
  , ( "roles",    form.roles    |> Field.value |> Set.toList |> Encode.list Encode.string )
  ] |> Encode.object

decodeForm : Decode.Value -> Form -> Form
decodeForm value =
  case
    ( value |> decode "state"    Decode.string
    , value |> decode "response" Data.decodeResponse
    , value |> decode "form"     Decode.value
    )
  of
    ( Just "edit", Just res, Just form ) -> toEdit res >> decodeFields form
    _ -> identity

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

toStatic : Form -> Form
toStatic form = { form | state = StaticState }

toEdit : Data.Response -> Form -> Form
toEdit res form =
  let
    body = res |> HttpView.body
  in
    { form | state = EditState False res }
    |> Form.set birthday_ (body |> get_birthday)
    |> Form.set start_at_ (body |> get_start_at)
    |> Form.set gender_   (body |> get_gender)
    |> Form.set quality_  (body |> get_quality)
    |> Form.set roles_    (body |> get_roles)

toCommit : Form -> Form
toCommit form =
  case form.state of
    EditState _ res -> { form | state = EditState True res }
    _ -> form

changed : Form -> Form
changed form =
  case form.state of
    EditState _ res -> { form | state = EditState False res }
    _ -> form

put : HttpView.Migration Response -> Form -> Form
put mig =
  if mig |> HttpView.isConflict
    then changed
    else identity


view : HttpView.Model Data.Response -> Form -> View
view http form =
  let
    error = "conflict"

    res = http |> HttpView.response

    data =
      ( case form.state of
        StaticState -> Nothing
        EditState _ r -> r |> HttpView.body |> Just

      , res |> Maybe.map HttpView.body
      )
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
  in
    { isStatic = (form.state == StaticState)
    , hasError = errors |> List.isEmpty |> not
    , state    = http |> HttpView.state
    , response = res
    , form     =
      { birthday = model.birthday |> expose form.state res
      , start_at = model.start_at |> expose form.state res
      , gender   = model.gender   |> expose form.state res
      , quality  = model.quality  |> expose form.state res
      , roles    = model.roles    |> expose form.state res
      }
    }

expose : EditState -> Maybe Data.Response -> Validate.Model (Conflict.Form Form a) -> State a
expose st res model =
  case model |> Validate.expose of
    (fieldName,form,errors) ->
      case st of
        StaticState -> Static fieldName
        EditState isCommit last ->
          if isCommit && ( last |> isDifferentResponse res )
            then Static fieldName
            else Edit   fieldName form ( form.field |> Conflict.state ) errors

isDifferentResponse : Maybe Data.Response -> Data.Response -> Bool
isDifferentResponse data last =
  case data of
    Nothing  -> False
    Just res -> ( res |> Data.etag ) /= ( last |> Data.etag )
