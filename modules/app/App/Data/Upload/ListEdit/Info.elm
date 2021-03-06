module GettoCodes.App.Data.Upload.ListEdit.Info exposing
  ( Msg
  , init
  , clear
  , subscriptions
  , update
  , content
  )
import GettoCodes.App.I18n as AppI18n
import GettoCodes.App.Data.Upload.I18n as I18n
import GettoCodes.App.Data.Upload.ListEdit.Model as Model
import GettoCodes.App.Data.Upload.ListEdit.Data.View as Data
import GettoCodes.App.Data.Upload.ListEdit.Info.View as View
import GettoCodes.App.Data.Upload.ListEdit.Info.Html as Html
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
import Getto.Html.Table as Table

type Msg
  = Edit    Data.Upload
  | Cancel  Data.Upload
  | Change  Data.Upload
  | Request Data.Upload
  | Input   Data.Upload (View.Prop String) String
  | Resolve Data.Upload (View.Prop String) (Conflict.Resolve String)
  | PutStateChanged Data.Upload (HttpView.Migration View.Response)
  | GetStateChanged Data.Upload (HttpView.Migration Data.Response)

signature = "info"

put : Data.Upload -> Http.Tracker Model.Frame View.Response
put row = Http.tracker ("put-" ++ (row.id |> String.fromInt)) <|
  \model ->
    let
      info = model |> Frame.app |> .search |> .info
    in
      Http.put
        { url      = "uploads/:id/info" |> Api.url [ ( "id", row.id |> String.fromInt ) ]
        , headers  = model |> Api.headers
        , params   = info  |> View.params row
        , response = View.response
        , timeout  = 10 * 1000
        }

putTrack   row = Http.track   signature (put row) (PutStateChanged row)
putRequest row = Http.request signature (put row) (PutStateChanged row)

get : Data.Upload -> Http.Tracker Model.Frame Data.Response
get row = Http.tracker ("get-" ++ (row.id |> String.fromInt)) <|
  \model ->
    Http.get
      { url      = "uploads/:id" |> Api.url [ ( "id", row.id |> String.fromInt ) ]
      , headers  = model |> Api.headers
      , params   = QueryEncode.null
      , response = Data.response
      , timeout  = 10 * 1000
      }

getTrack   row = Http.track   signature (get row) (GetStateChanged row)
getRequest row = Http.request signature (get row) (GetStateChanged row)


init : String -> View.Units
init = View.empty

clear : View.Units -> View.Units
clear = View.clear

subscriptions : View.Units -> Sub Msg
subscriptions model =
  model
  |> View.rows
  |> List.map putTrack
  |> Sub.batch

update : Msg -> View.Units -> ( View.Units, Model.Transition Msg )
update msg model =
  case msg of
    Edit    row -> ( model |> View.updateForm row (edit row),  fillAndFixedMidashi row )
    Cancel  row -> ( model |> View.updateForm row Edit.cancel, Frame.fixedMidashi  )
    Change  row -> ( model |> View.updateForm row Edit.change, T.none         )
    Request row -> ( model |> View.updateForm row Edit.commit, putRequest row )

    Input   row prop value -> ( model |> View.updateForm row (Form.set prop value),       T.none )
    Resolve row prop mig   -> ( model |> View.updateForm row (Conflict.resolve prop mig), fill row )

    PutStateChanged row mig ->
      ( model
        |> View.update row
          (\unit -> { unit | put = unit.put |> HttpView.update mig })
      , if mig |> HttpView.isComplete
        then getRequest row
        else T.none
      )

    GetStateChanged row mig ->
      ( model
        |> View.update row
          (\unit -> { unit | get  = unit.get  |> HttpView.update mig } )
      , if mig |> HttpView.isComplete
        then Frame.fixedMidashi
        else T.none
      )

edit : Data.Upload -> View.Form -> View.Form
edit = Data.http >> HttpView.response >> Edit.edit View.edit

fill : Data.Upload -> Model.Transition Msg
fill row = Frame.app >> .search >> .info >> View.get row >> .form >> Edit.fields >> Html.pairs >> Dom.fill

fillAndFixedMidashi : Data.Upload -> Model.Transition Msg
fillAndFixedMidashi row =
  [ fill row
  , Frame.fixedMidashi
  ] |> T.batch

content : View.Units -> Table.Column Data.Upload Msg
content model = Html.info
  { view = model |> View.view
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
    , edit    = Edit
    , cancel  = Cancel
    , input   = Input
    , resolve = Resolve
    }
  , i18n =
    { field = I18n.field
    , error = I18n.error
    , form  = AppI18n.form
    , http  = AppI18n.http
    }
  }
