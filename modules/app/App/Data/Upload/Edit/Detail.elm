module GettoUpload.App.Data.Upload.Edit.Detail exposing
  ( Msg
  , init
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  , contents
  )
import GettoUpload.App.Data.Upload.Edit.Model as Model
import GettoUpload.App.Data.Upload.Edit.Detail.View as View
import GettoUpload.App.Data.Upload.Edit.Detail.Html as Html
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Page.Options.View as Options
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Http as Http
import GettoUpload.Command.Dom as Dom
import GettoUpload.View.Http as HttpView
import GettoUpload.I18n.App as AppI18n
import GettoUpload.I18n.App.Data.Upload as I18n
import GettoUpload.I18n.Http as HttpI18n

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Edit as Edit
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict

import Json.Encode as Encode
import Json.Decode as Decode

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

type Msg
  = Edit
  | Cancel
  | Change
  | Request
  | Input      (View.Prop String)       String
  | Toggle     (View.Prop (Set String)) String
  | Resolve    (View.Prop String)       (Conflict.Resolve String)
  | ResolveSet (View.Prop (Set String)) (Conflict.Resolve (Set String))
  | StateChanged (HttpView.Migration View.Response)

signature = "detail"

put : Http.Tracker Model.Frame View.Response
put = Http.tracker "put" <|
  \model ->
    let
      data   = model |> Frame.app |> .data
      detail = model |> Frame.app |> .detail
    in
      Http.put
        { url      = "upload/:id/detail" |> Api.url ( data |> Model.pathInfo )
        , headers  = model |> Api.headers
        , params   = detail.form |> View.params data.get
        , response = View.response
        , timeout  = 10 * 1000
        }

putTrack   = Http.track   signature put StateChanged
putRequest = Http.request signature put StateChanged


init : Frame.InitModel -> ( Model.Detail, Model.Transition Msg )
init model =
  ( { form = signature |> View.init
    , put  = HttpView.empty
    }
  , fill
  )

encodeStore : Model.Detail -> Encode.Value
encodeStore model = model.form |> View.encodeForm

decodeStore : Decode.Value -> Model.Detail -> Model.Detail
decodeStore value model = { model | form = model.form |> View.decodeForm value }

subscriptions : Model.Detail -> Sub Msg
subscriptions model = putTrack

update : Model.Data -> Msg -> Model.Detail -> ( Model.Detail, ( Model.Transition Msg, Bool ) )
update data msg model =
  case msg of
    Edit    -> ( { model | form = model.form |> Edit.edit View.edit data.get }, ( fillAndStore,   False ) )
    Cancel  -> ( { model | form = model.form |> Edit.cancel },                  ( Frame.storeApp, False ) )
    Change  -> ( { model | form = model.form |> Edit.change },                  ( Frame.storeApp, False ) )
    Request -> ( { model | form = model.form |> Edit.commit },                  ( putRequest,     False ) )

    Input  prop value -> ( { model | form = model.form |> Form.set    prop value }, ( T.none, False ) )
    Toggle prop value -> ( { model | form = model.form |> Form.toggle prop value }, ( T.none, False ) )

    Resolve    prop mig -> ( { model | form = model.form |> Conflict.resolve prop mig }, ( fillAndStore, False ) )
    ResolveSet prop mig -> ( { model | form = model.form |> Conflict.resolve prop mig }, ( fillAndStore, False ) )

    StateChanged mig ->
      ( { model
        | put  = model.put  |> HttpView.update mig
        , form = model.form |> Edit.put mig
        }
      , ( T.none, True )
      )

fill : Model.Transition Msg
fill = Frame.app >> .detail >> .form >> Edit.fields >> Html.pairs >> Dom.fill

fillAndStore : Model.Transition Msg
fillAndStore = [ fill, Frame.storeApp ] |> T.batch


contents : Model.Frame -> List (Html Msg)
contents model =
  [ model |> content
  ]

content : Model.Frame -> Html Msg
content model = L.lazy3
  (\options data detail -> Html.detail
    { view = detail.form |> View.view
    , get  = data.get
    , put  = detail.put
    , options =
      { gender  = options.get |> Options.gender  |> toSelectOptions I18n.gender
      , quality = options.get |> Options.quality |> toBoxOptions I18n.quality
      , role    = options.get |> Options.role    |> toBoxOptions AppI18n.role
      }
    , msg =
      { put        = Request
      , input      = Input
      , toggle     = Toggle
      , resolve    = Resolve
      , resolveSet = ResolveSet
      , change     = Change
      , edit       = Edit
      , cancel     = Cancel
      }
    , i18n =
      { title   = I18n.title
      , field   = I18n.field
      , error   = I18n.error
      , form    = AppI18n.form
      , http    = HttpI18n.error
      , gender  = I18n.gender
      , quality = I18n.quality
      , role    = AppI18n.role
      }
    }
  )
  (model |> Frame.layout |> .options)
  (model |> Frame.app    |> .data)
  (model |> Frame.app    |> .detail)

toSelectOptions : (String -> String) -> Maybe (List String) -> List ( String, String )
toSelectOptions i18n = Maybe.map (toOptions i18n >> includeDefault) >> withLoading

toBoxOptions : (String -> String) -> Maybe (List String) -> List ( String, String )
toBoxOptions i18n = Maybe.map (toOptions i18n) >> Maybe.withDefault []

toOptions : (String -> String) -> List String -> List ( String, String )
toOptions i18n = List.map (\value -> ( value, value |> i18n ) )

includeDefault : List ( String, String ) -> List ( String, String )
includeDefault options = ( "", "please-select" |> AppI18n.form ) :: options

withLoading : Maybe (List ( String, String )) -> List ( String, String )
withLoading = Maybe.withDefault [ ( "", "loading" |> AppI18n.form ) ]