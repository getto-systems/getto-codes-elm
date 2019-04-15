module GettoCodes.App.Data.Upload.ListEdit.Detail exposing
  ( Msg
  , edit
  , subscriptions
  , update
  , content
  )
import GettoCodes.App.I18n as AppI18n
import GettoCodes.App.Data.Upload.I18n as I18n
import GettoCodes.App.Data.Upload.ListEdit.Model as Model
import GettoCodes.App.Data.Upload.ListEdit.Data.View as Data
import GettoCodes.App.Data.Upload.ListEdit.Detail.View as View
import GettoCodes.App.Data.Upload.ListEdit.Detail.Html as Html
import GettoCodes.Layout.Frame as Frame
import GettoCodes.Layout.Api as Api
import GettoCodes.Command.Http as Http
import GettoCodes.Command.Dom as Dom
import GettoCodes.View.Http as HttpView

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Field.Form as Form
import Getto.Field.Edit as Edit
import Getto.Field.Conflict as Conflict

import Json.Encode as Encode

import Html as H exposing ( Html )
import Html.Lazy as L

type Msg
  = Edit Data.Upload
  | Cancel
  | Change
  | Request
  | Input   (View.Prop String) String
  | Resolve (View.Prop String) (Conflict.Resolve String)
  | PutStateChanged (HttpView.Migration View.Response)
  | GetStateChanged (HttpView.Migration Data.Response)

signature = "detail"

put : Http.Tracker Model.Frame View.Response
put = Http.tracker "put" <|
  \model ->
    let
      detail = model |> Frame.app |> .search |> .detail
      id = detail |> Maybe.map (.row >> .id) |> Maybe.withDefault 0
    in
      Http.put
        { url      = "uploads/:id/detail" |> Api.url [ ( "id", id |> String.fromInt ) ]
        , headers  = model |> Api.headers
        , params   = detail |> Maybe.map View.params |> Maybe.withDefault Encode.null
        , response = View.response
        , timeout  = 10 * 1000
        }

putTrack   = Http.track   signature put PutStateChanged
putRequest = Http.request signature put PutStateChanged

get : Http.Tracker Model.Frame Data.Response
get = Http.tracker "get" <|
  \model ->
    let
      detail = model |> Frame.app |> .search |> .detail
      id = detail |> Maybe.map (.row >> .id) |> Maybe.withDefault 0
    in
      Http.get
        { url      = "uploads/:id" |> Api.url [ ( "id", id |> String.fromInt ) ]
        , headers  = model |> Api.headers
        , params   = QueryEncode.null
        , response = Data.response
        , timeout  = 10 * 1000
        }

getTrack   = Http.track   signature get GetStateChanged
getRequest = Http.request signature get GetStateChanged


edit : Data.Upload -> Msg
edit = Edit

subscriptions : Maybe View.Unit -> Sub Msg
subscriptions model = putTrack

update : Msg -> Maybe View.Unit -> ( Maybe View.Unit, Model.Transition Msg )
update msg model =
  case msg of
    Edit row -> ( View.init signature row (toEdit row), fill )
    Cancel   -> ( Nothing, T.none )
    Change   -> ( model |> View.updateForm Edit.change, T.none )
    Request  -> ( model |> View.updateForm Edit.commit, putRequest )

    Input   prop value -> ( model |> View.updateForm (Form.set prop value),       T.none )
    Resolve prop mig   -> ( model |> View.updateForm (Conflict.resolve prop mig), fill )

    PutStateChanged mig ->
      ( model
        |> Maybe.map
          (\unit -> { unit | put = unit.put |> HttpView.update mig })
      , if mig |> HttpView.isComplete
        then getRequest
        else T.none
      )

    GetStateChanged mig ->
      ( model
        |> Maybe.map
          (\unit -> { unit | get = unit.get |> HttpView.update mig } )
      , T.none
      )

toEdit : Data.Upload -> View.Form -> View.Form
toEdit = Data.http >> HttpView.response >> Edit.edit View.edit

fill : Model.Transition Msg
fill = Frame.app >> .search >> .detail >>
  (\detail ->
    case detail of
      Just unit -> unit.form |> Edit.fields |> Html.pairs |> Dom.fill
      Nothing   -> Cmd.none
  )

content : Model.Frame -> Html Msg
content model = L.lazy
  (\detail ->
    Html.detail
      { view = detail |> Maybe.map View.view
      , options =
        { gender =
          [ ( "", "please-select" |> AppI18n.form )
          , ( "male",   "male"   |> I18n.gender )
          , ( "female", "female" |> I18n.gender )
          , ( "other",  "other"  |> I18n.gender )
          ]
        }
      , msg =
        { put     = Request
        , change  = Change
        , cancel  = Cancel
        , input   = Input
        , resolve = Resolve
        }
      , i18n =
        { title = I18n.title
        , field = I18n.field
        , error = I18n.error
        , form  = AppI18n.form
        , http  = AppI18n.http
        }
      }
  )
  (model |> Frame.app |> .search |> .detail)
