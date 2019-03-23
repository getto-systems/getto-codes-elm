module GettoCodes.App.Data.Upload.ListEdit.Info.Html exposing
  ( info
  , pairs
  )
import GettoCodes.App.Data.Upload.ListEdit.Data.View as Data
import GettoCodes.App.Data.Upload.ListEdit.Info.View as Info
import GettoCodes.View.Html as Html
import GettoCodes.View.Html.Button as Button
import GettoCodes.View.Html.Input as Input
import GettoCodes.View.Html.Http as Http
import GettoCodes.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Edit as Edit
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
      , content = \(upload,v) ->
        case (v.view.isStatic,v.view.form.name) of
          (True,(name,form,opts)) ->
            Table.td []
              [ H.p [] [ form.last |> H.text ]
              ]
          (False,(name,form,opts)) ->
            Table.td ( opts.errors |> Input.isError ) <| List.concat
              [ [ form.field |> Input.text []
                  (model.msg.input upload form.prop) (model.msg.change upload)
                ]
              , form |> Conflict.state |> Input.conflict model.i18n.form (model.msg.resolve upload form.prop)
              , opts.errors |> Input.errors model.i18n.error
              ]
      }
    , Table.column ( Table.none, Table.none )
      { header  = Table.th [] [ "gender" |> model.i18n.field |> H.text ]
      , summary = Table.empty
      , content = \(upload,v) ->
        case (v.view.isStatic,v.view.form.gender) of
          (True,(name,form,opts)) ->
            Table.td []
              [ H.p [] [ form.last |> H.text ]
              ]
          (False,(name,form,opts)) ->
            Table.td ( opts.errors |> Input.isError ) <| List.concat
              [ [ form.field |> Input.select model.options.gender []
                  (model.msg.input upload form.prop) (model.msg.change upload)
                ]
              , form |> Conflict.state |> Input.conflict model.i18n.form (model.msg.resolve upload form.prop)
              , opts.errors |> Input.errors model.i18n.error
              ]
      }
    , Table.column ( Table.none, Table.single )
      { header  = Table.empty
      , summary = Table.empty
      , content = \(upload,v) ->
        if v.view.isStatic
          then
            Table.td []
              [ "edit" |> model.i18n.form |> Button.edit (model.msg.edit upload)
              ]
          else
            case v.put |> HttpView.state of
              HttpView.Connecting progress ->
                Table.td []
                  [ "saving" |> model.i18n.form |> Button.connecting
                  ]
              HttpView.Ready response ->
                Table.td []
                  [ H.form [ model.msg.put upload |> E.onSubmit ]
                    [ case v.get |> HttpView.state of
                      HttpView.Connecting _ -> Html.spinner
                      HttpView.Ready _ ->
                        case v.view.state of
                          Edit.Same -> "" |> H.text
                          Edit.HasError    -> "has-error" |> model.i18n.form |> Button.error
                          Edit.HasModified -> "save"      |> model.i18n.form |> Button.save
                    , " " |> H.text
                    , "cancel" |> model.i18n.form |> Button.cancel (model.msg.cancel upload)
                    , response |> Http.error model.i18n.http
                    ]
                  ]
      }
    ]

pairs : Info.Fields -> List ( String, String )
pairs fields =
  [ fields.name |> Field.id_value
  ]
