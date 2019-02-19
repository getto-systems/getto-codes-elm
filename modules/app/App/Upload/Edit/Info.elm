module GettoUpload.App.Upload.Edit.Info exposing
  ( Model
  , Msg
  , init
  , query
  , queryChanged
  , store
  , storeChanged
  , subscriptions
  , update
  , contents
  , dialogs
  )
import GettoUpload.App.Upload.Edit.Info.View as View
import GettoUpload.App.Upload.Edit.Info.Html as Html
import GettoUpload.Layout.Page.Page as Layout
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Http as Http
import GettoUpload.Command.Dom as Dom
import GettoUpload.View.Http as HttpView
import GettoUpload.I18n.App as AppI18n
import GettoUpload.I18n.App.Upload.Edit.Info as I18n
import GettoUpload.I18n.Http as HttpI18n
import GettoUpload.Extension.Href as Href
import GettoUpload.Extension.Href.Upload as Upload

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Http.Part as Part
import Getto.Json.SafeDecode as SafeDecode
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate

import Browser.Navigation as Navigation
import File exposing ( File )
import File.Select
import Set exposing ( Set )
import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias FrameModel a = Frame.Model Layout.Model { a | info : Model }
type alias FrameTransition a = Transition (FrameModel a) Msg
type alias Model =
  { signature : String
  , id   : Int
  , form : View.Form
  , get : HttpView.Model View.ResponseHeader View.ResponseBody
  , put : HttpView.Model View.ResponseHeader View.ResponseBody
  }

type Msg
  = Edit
  | Static
  | FieldInput (Form.Prop View.Form String) String
  | FieldCheck (Form.Prop View.Form (Set String)) String
  | FieldChange
  | FileRequest (Form.Prop View.Form (List File))
  | FileSelect (Form.Prop View.Form (List File)) File
  | GetStateChanged (HttpView.Migration View.ResponseHeader View.ResponseBody)
  | PutRequest
  | PutStateChanged (HttpView.Migration View.ResponseHeader View.ResponseBody)

init : String -> Frame.InitModel -> ( Model, FrameTransition a )
init signature model =
  ( { signature = signature
    , id = 0
    , form =
      { state    = View.Static
      , name     = Field.init signature "name"     ""
      , memo     = Field.init signature "memo"     ""
      , age      = Field.init signature "age"      ""
      , email    = Field.init signature "email"    ""
      , tel      = Field.init signature "tel"      ""
      , birthday = Field.init signature "birthday" ""
      , start_at = Field.init signature "start_at" ""
      , gender   = Field.init signature "gender"   ""
      , quality  = Field.init signature "quality"  ""
      , roles    = Field.init signature "roles"    Set.empty
      }
    , get = HttpView.empty
    , put = HttpView.empty
    }
  , [ Http.request signature get GetStateChanged
    ] |> Transition.batch
  )

fill : FrameModel a -> Cmd msg
fill = Frame.app >> .info >>
  (\m -> Dom.fill
    [ m.form.name     |> Dom.string
    , m.form.memo     |> Dom.string
    , m.form.age      |> Dom.string
    , m.form.email    |> Dom.string
    , m.form.tel      |> Dom.string
    , m.form.birthday |> Dom.string
    , m.form.start_at |> Dom.string
    , m.form.gender   |> Dom.string
    ]
  )

get : Http.Tracker (FrameModel a) View.ResponseHeader View.ResponseBody
get = Http.tracker "get" <|
  \model ->
    let
      m = model |> Frame.app |> .info
    in
      Http.get
        { url     = "upload/:id" |> Api.url ( m |> pathInfo )
        , headers = model |> Api.headers
        , params  = QueryEncode.empty
        , response = response
        , timeout = 10 * 1000
        }

put : Http.Tracker (FrameModel a) View.ResponseHeader View.ResponseBody
put = Http.tracker "put" <|
  \model ->
    let
      m = model |> Frame.app |> .info
    in
      Http.put
        { url     = "upload/:id/info" |> Api.url ( m |> pathInfo )
        , headers = model |> Api.headers
        , params  = Encode.object
          [ ( "name",      m.form.name     |> Field.value |> Encode.string )
          , ( "memo",      m.form.memo     |> Field.value |> Encode.string )
          , ( "age",       m.form.age      |> Field.value |> Encode.string )
          , ( "email",     m.form.email    |> Field.value |> Encode.string )
          , ( "tel",       m.form.tel      |> Field.value |> Encode.string )
          , ( "birthday",  m.form.birthday |> Field.value |> Encode.string )
          , ( "start_at",  m.form.start_at |> Field.value |> Encode.string )
          , ( "gender",    m.form.gender   |> Field.value |> Encode.string )
          , ( "quality",   m.form.quality  |> Field.value |> Encode.string )
          , ( "roles",     m.form.roles    |> Field.value |> Set.toList |> Encode.list Encode.string )
          ]
        , response = response
        , timeout = 10 * 1000
        }

pathInfo : Model -> List ( String, String )
pathInfo model =
  [ ( "id", model.id |> String.fromInt )
  ]

response =
  { header = HeaderDecode.succeed ()
  , body = Decode.map2 View.ResponseBody
    ( Decode.at ["info"]
      ( Decode.map5 View.ResponseInfo
        (Decode.at ["name"]  Decode.string)
        (Decode.at ["memo"]  Decode.string)
        (Decode.at ["age"]   Decode.int)
        (Decode.at ["email"] Decode.string)
        (Decode.at ["tel"]   Decode.string)
      )
    )
    ( Decode.at ["detail"]
      ( Decode.map5 View.ResponseDetail
        (Decode.at ["birthday"] Decode.string)
        (Decode.at ["start_at"] Decode.string)
        (Decode.at ["gender"]   Decode.string)
        (Decode.at ["quality"]  Decode.string)
        (Decode.at ["roles"]   (Decode.string |> Decode.list |> Decode.map Set.fromList))
      )
    )
  }

query : Model -> QueryEncode.Value
query model = QueryEncode.object
  [ ( "id", model.id |> QueryEncode.int )
  ]

queryChanged : List String -> QueryDecode.Value -> Model -> Model
queryChanged names value model =
  let
    entryAt name = QueryDecode.entryAt (names ++ [name])
  in
    { model | id = value |> entryAt "id" (QueryDecode.int 0) }

store : Model -> Encode.Value
store model = Encode.object
  [ ( "name",     model.form.name     |> Field.value |> Encode.string )
  , ( "memo",     model.form.memo     |> Field.value |> Encode.string )
  , ( "age",      model.form.age      |> Field.value |> Encode.string )
  , ( "email",    model.form.email    |> Field.value |> Encode.string )
  , ( "tel",      model.form.tel      |> Field.value |> Encode.string )
  , ( "birthday", model.form.birthday |> Field.value |> Encode.string )
  , ( "start_at", model.form.start_at |> Field.value |> Encode.string )
  , ( "gender",   model.form.gender   |> Field.value |> Encode.string )
  , ( "quality",  model.form.quality  |> Field.value |> Encode.string )
  , ( "roles",    model.form.roles    |> Field.value |> Set.toList |> Encode.list Encode.string )
  ]

storeChanged : Decode.Value -> Model -> Model
storeChanged value model =
  { model
  | form =
    model.form
    |> Form.set name_     ( value |> SafeDecode.at ["name"]     (SafeDecode.string "") )
    |> Form.set memo_     ( value |> SafeDecode.at ["memo"]     (SafeDecode.string "") )
    |> Form.set age_      ( value |> SafeDecode.at ["age"]      (SafeDecode.string "") )
    |> Form.set email_    ( value |> SafeDecode.at ["email"]    (SafeDecode.string "") )
    |> Form.set tel_      ( value |> SafeDecode.at ["tel"]      (SafeDecode.string "") )
    |> Form.set birthday_ ( value |> SafeDecode.at ["birthday"] (SafeDecode.string "") )
    |> Form.set start_at_ ( value |> SafeDecode.at ["start_at"] (SafeDecode.string "") )
    |> Form.set gender_   ( value |> SafeDecode.at ["gender"]   (SafeDecode.string "") )
    |> Form.set quality_  ( value |> SafeDecode.at ["quality"]  (SafeDecode.string "") )
    |> Form.set roles_    ( value |> SafeDecode.at ["roles"]    (SafeDecode.list (SafeDecode.string "")) |> Set.fromList )
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  [ Http.track model.signature get GetStateChanged
  , Http.track model.signature put PutStateChanged
  ] |> Sub.batch

update : Msg -> Model -> ( Model, FrameTransition a )
update msg model =
  case msg of
    Edit ->
      ( { model | form = model.form |> edit model |> View.edit }
      , fill
      )
    Static ->
      ( { model | form = model.form |> View.static }
      , Transition.none
      )

    FieldInput prop value ->
      ( { model | form = model.form |> Form.set prop value }
      , Transition.none
      )
    FieldCheck prop value ->
      ( { model | form = model.form |> Form.toggle prop value }
      , Transition.none
      )
    FieldChange -> ( model, Frame.storeApp )

    FileRequest prop -> ( model, always ( FileSelect prop |> File.Select.file [] ) )
    FileSelect prop file ->
      ( { model | form = model.form |> Form.set prop [file] }
      , Transition.none
      )

    GetStateChanged mig ->
      ( { model | get = model.get |> HttpView.update mig }
      , Transition.none
      )

    PutRequest -> ( model, Http.request model.signature put PutStateChanged )
    PutStateChanged mig ->
      ( { model
        | put  = model.put  |> HttpView.update mig
        , form = model.form |> View.done mig
        }
      , Transition.none
      )

edit : Model -> View.Form -> View.Form
edit model form =
  let
    res =
      [ model.put, model.get ]
      |> List.filterMap (HttpView.response >> Maybe.map HttpView.body)
      |> List.head
  in
    case res of
      Nothing -> form
      Just body ->
        form
        |> Form.set name_     body.info.name
        |> Form.set memo_     body.info.memo
        |> Form.set age_     (body.info.age |> String.fromInt)
        |> Form.set email_    body.info.email
        |> Form.set tel_      body.info.tel
        |> Form.set birthday_ body.detail.birthday
        |> Form.set start_at_ body.detail.start_at
        |> Form.set gender_   body.detail.gender
        |> Form.set quality_  body.detail.quality
        |> Form.set roles_    body.detail.roles

contents : FrameModel a -> List (Html Msg)
contents model =
  [ H.section [ A.class "edit" ]
    [ model |> info
    ]
  ]

name_     = Form.prop .name     (\v m -> { m | name     = v })
memo_     = Form.prop .memo     (\v m -> { m | memo     = v })
age_      = Form.prop .age      (\v m -> { m | age      = v })
email_    = Form.prop .email    (\v m -> { m | email    = v })
tel_      = Form.prop .tel      (\v m -> { m | tel      = v })
birthday_ = Form.prop .birthday (\v m -> { m | birthday = v })
start_at_ = Form.prop .start_at (\v m -> { m | start_at = v })
gender_   = Form.prop .gender   (\v m -> { m | gender   = v })
quality_  = Form.prop .quality  (\v m -> { m | quality  = v })
roles_    = Form.prop .roles    (\v m -> { m | roles    = v })

info : FrameModel a -> Html Msg
info model = L.lazy
  (\m -> Html.info
    { title = "info"
    , form = m.form |> View.compose
      { name     = ( name_,     [ m.form.name |> Validate.blank "blank" ] )
      , memo     = ( memo_,     [] )
      , age      = ( age_,      [] )
      , email    = ( email_,    [] )
      , tel      = ( tel_,      [] )
      , birthday = ( birthday_, [] )
      , start_at = ( start_at_, [] )
      , gender   = ( gender_,   [] )
      , quality  = ( quality_,  [] )
      , roles    = ( roles_,    [] )
      }
    , get = m.get
    , put = m.put
    , options =
      { gender =
        [ ( "", "please-select" |> AppI18n.form )
        , ( "male",   "male"   |> I18n.gender )
        , ( "female", "female" |> I18n.gender )
        , ( "other",  "other"  |> I18n.gender )
        ]
      , quality =
        [ ( "high", "high" |> I18n.quality )
        , ( "low",  "low"  |> I18n.quality )
        ]
      , roles =
        [ ( "admin",  "admin"  |> AppI18n.role )
        , ( "upload", "upload" |> AppI18n.role )
        ]
      }
    , msg =
      { put    = PutRequest
      , input  = FieldInput
      , check  = FieldCheck
      , change = FieldChange
      , edit   = Edit
      , static = Static
      }
    , i18n =
      { title = I18n.title
      , field = I18n.field
      , error = I18n.error
      , form  = AppI18n.form
      , http  = HttpI18n.error
      }
    }
  )
  (model |> Frame.app |> .info)

dialogs : FrameModel a -> List (Html Msg)
dialogs model = []
