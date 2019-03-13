module GettoUpload.App.Data.Upload.ListEdit.Info.Html exposing
  ( info
  , pairs
  )
import GettoUpload.App.Data.Upload.ListEdit.Data.View as Data
import GettoUpload.App.Data.Upload.ListEdit.Info.View as Info
import GettoUpload.View.Html as Html
import GettoUpload.View.Html.Button as Button
import GettoUpload.View.Html.Input as Input
import GettoUpload.View.Html.Http as Http
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Edit as Edit exposing ( State(..), AggregateState(..) )
import Getto.Field.Conflict as Conflict
import Getto.Html.Table as Table

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias InfoModel msg =
  { view : Info.View
  , options :
    { gender : List ( String, String )
    }
  , msg :
    { put     : Data.Upload -> msg
    , change  : Data.Upload -> msg
    , edit    : Data.Upload -> msg
    , cancel  : Data.Upload -> msg
    , input   : Data.Upload -> Info.Prop String -> String -> msg
    , resolve : Data.Upload -> Info.Prop String -> Conflict.Resolve String -> msg
    }
  , i18n :
    { field : String -> String
    , error : String -> String
    , form  : String -> String
    , http  : HttpView.Error -> String
    }
  }

info : InfoModel msg -> Table.Column Data.Upload msg
info model =
  Table.rows ( \upload -> [ upload |> model.view ] )
    [ Table.column ( Table.none, Table.none )
      { header  = Table.th [] [ "name" |> model.i18n.field |> H.text ]
      , summary = Table.empty
      , content = \(upload,view) ->
        case view.form.name of
          Static name value ->
            Table.td []
              [ H.p [] [ value |> H.text ]
              ]
          Edit name form conflict errors ->
            Table.td ( errors |> Input.isError ) <| List.concat
              [ [ form.field |> Input.text []
                  (model.msg.input upload form.prop) (model.msg.change upload)
                ]
              , conflict |> Input.conflict model.i18n.form (model.msg.resolve upload form.prop)
              , errors   |> Input.errors   model.i18n.error
              ]
      }
    , Table.column ( Table.none, Table.none )
      { header  = Table.th [] [ "gender" |> model.i18n.field |> H.text ]
      , summary = Table.empty
      , content = \(upload,view) ->
        case view.form.gender of
          Static name value ->
            Table.td []
              [ H.p [] [ value |> H.text ]
              ]
          Edit name form conflict errors ->
            Table.td ( errors |> Input.isError ) <| List.concat
              [ [ form.field |> Input.select model.options.gender []
                  (model.msg.input upload form.prop) (model.msg.change upload)
                ]
              , conflict |> Input.conflict model.i18n.form (model.msg.resolve upload form.prop)
              , errors   |> Input.errors   model.i18n.error
              ]
      }
    , Table.column ( Table.none, Table.single )
      { header  = Table.empty
      , summary = Table.empty
      , content = \(upload,view) ->
        if view.isStatic
          then
            Table.td []
              [ "edit" |> model.i18n.form |> Button.edit (model.msg.edit upload)
              ]
          else
            case view.put |> HttpView.state of
              HttpView.Connecting progress ->
                Table.td []
                  [ "saving" |> model.i18n.form |> Button.connecting
                  ]
              HttpView.Ready response ->
                Table.td []
                  [ H.form [ model.msg.put upload |> E.onSubmit ]
                    [ case view.get |> HttpView.state of
                      HttpView.Connecting _ -> Html.spinner
                      HttpView.Ready _ ->
                        case view.state of
                          Same -> "" |> H.text
                          HasError    -> "has-error" |> model.i18n.form |> Button.error
                          HasModified -> "save"      |> model.i18n.form |> Button.save
                    , " " |> H.text
                    , "cancel" |> model.i18n.form |> Button.cancel (model.msg.cancel upload)
                    , response |> Http.error model.i18n.http
                    ]
                  ]
      }
    ]

pairs : Info.Fields -> List ( String, String )
pairs fields =
  [ fields.name |> Field.pair
  ]
