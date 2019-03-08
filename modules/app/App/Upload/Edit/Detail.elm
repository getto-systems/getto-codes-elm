module GettoUpload.App.Upload.Edit.Detail exposing
  ( Model
  , Msg
  , init
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  , contents
  , dialogs
  )
import GettoUpload.App.Upload.Edit.Data as Data
import GettoUpload.App.Upload.Edit.Detail.View as View
import GettoUpload.App.Upload.Edit.Detail.Html as Html
import GettoUpload.Layout.Page.Page as Layout
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Http as Http
import GettoUpload.Command.Dom as Dom
import GettoUpload.View.Http as HttpView
import GettoUpload.I18n.App as AppI18n
import GettoUpload.I18n.App.Upload as I18n
import GettoUpload.I18n.Http as HttpI18n
import GettoUpload.Extension.Href as Href
import GettoUpload.Extension.Href.Upload as Upload

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Url.Query.SafeDecode as QuerySafeDecode
import Getto.Json.SafeDecode as SafeDecode
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict

import Json.Encode as Encode
import Json.Decode as Decode

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

type alias FrameModel a = Frame.Model Layout.Model { a | data : Data.Model, detail : Model }
type alias FrameTransition a = Transition (FrameModel a) Msg
type alias Model =
  { form : View.Form
  , put  : HttpView.Model View.Response
  }

type Msg
  = Edit
  | Static
  | Change
  | Request
  | Input      (View.Prop String)       String
  | Toggle     (View.Prop (Set String)) String
  | Resolve    (View.Prop String)       (Conflict.Resolve String)
  | ResolveSet (View.Prop (Set String)) (Conflict.Resolve (Set String))
  | StateChanged (HttpView.Migration View.Response)

signature = "detail"

put : Http.Tracker (FrameModel a) View.Response
put = Http.tracker "put" <|
  \model ->
    let
      data = model |> Frame.app |> .data
      m    = model |> Frame.app |> .detail
    in
      Http.put
        { url      = "upload/:id/detail" |> Api.url ( data |> Data.pathInfo )
        , headers  = model  |> Api.headers
        , params   = m.form |> View.params data.get
        , response = View.response
        , timeout  = 10 * 1000
        }

putTrack   = Http.track   signature put StateChanged
putRequest = Http.request signature put StateChanged


init : Frame.InitModel -> ( Model, FrameTransition a )
init model =
  ( { form = signature |> View.init
    , put  = HttpView.empty
    }
  , fill
  )

encodeStore : Data.Model -> Model -> Encode.Value
encodeStore data model = Encode.object
  [ ( data.id |> String.fromInt
    , model.form |> View.encodeForm
    )
  ]

decodeStore : Data.Model -> Decode.Value -> Model -> Model
decodeStore data value model =
  { model
  | form = model.form |> View.decodeForm
    (value |> SafeDecode.valueAt [data.id |> String.fromInt])
  }

subscriptions : Model -> Sub Msg
subscriptions model = putTrack

update : Data.Model -> Msg -> Model -> ( Model, ( Data.FrameTransition a, FrameTransition a ) )
update data msg model =
  case msg of
    Edit    -> ( { model | form = model.form |> View.toEdit data.get }, ( T.none, fillAndStore   ) )
    Static  -> ( { model | form = model.form |> View.toStatic },        ( T.none, Frame.storeApp ) )
    Change  -> ( { model | form = model.form |> View.change },          ( T.none, Frame.storeApp ) )
    Request -> ( { model | form = model.form |> View.commit },          ( T.none, putRequest     ) )

    Input  prop value -> ( { model | form = model.form |> Form.set    prop value }, ( T.none, T.none ) )
    Toggle prop value -> ( { model | form = model.form |> Form.toggle prop value }, ( T.none, T.none ) )

    Resolve    prop mig -> ( { model | form = model.form |> Conflict.resolve prop mig }, ( T.none, fillAndStore ) )
    ResolveSet prop mig -> ( { model | form = model.form |> Conflict.resolve prop mig }, ( T.none, fillAndStore ) )

    StateChanged mig ->
      ( { model
        | put  = model.put  |> HttpView.update mig
        , form = model.form |> View.put mig
        }
      , ( mig |> Data.getRequestIfComplete, T.none )
      )

fill : FrameTransition a
fill = Frame.app >> .detail >> .form >> View.pairs >> Dom.fill

fillAndStore : FrameTransition a
fillAndStore = [ fill, Frame.storeApp ] |> T.batch


contents : FrameModel a -> List (Html Msg)
contents model =
  [ model |> detail
  ]

detail : FrameModel a -> Html Msg
detail model = L.lazy2
  (\data m -> Html.detail
    { title = signature
    , view = m.form |> View.view data.get
    , put  = m.put  |> HttpView.state
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
      { put        = Request
      , input      = Input
      , toggle     = Toggle
      , resolve    = Resolve
      , resolveSet = ResolveSet
      , change     = Change
      , edit       = Edit
      , static     = Static
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
  (model |> Frame.app |> .data)
  (model |> Frame.app |> .detail)

dialogs : FrameModel a -> List (Html Msg)
dialogs model = []
