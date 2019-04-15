module GettoCodes.App.Data.Upload.Edit.Info.Html exposing
  ( info
  , pairs
  )
import GettoCodes.App.Data.Upload.Edit.Data.View as Data
import GettoCodes.App.Data.Upload.Edit.Info.View as Info
import GettoCodes.View.Http as HttpView
import GettoCodes.Html.Icon as Icon
import GettoCodes.Html.Content as Content
import GettoCodes.Html.Button as Button
import GettoCodes.Html.Input as Input
import GettoCodes.Html.Http as Http

import Getto.Field as Field
import Getto.Field.Edit as Edit
import Getto.Field.Conflict as Conflict

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias InfoModel msg =
  { view : Maybe Info.View
  , get  : HttpView.Model Data.Response
  , put  : HttpView.Model Info.Response
  , msg :
    { put     : msg
    , change  : msg
    , edit    : msg
    , cancel  : msg
    , input   : Info.Prop String -> String -> msg
    , resolve : Info.Prop String -> Conflict.Resolve String -> msg
    }
  , i18n :
    { title : String -> String
    , field : String -> String
    , error : String -> String
    , form  : String -> String
    , http  : HttpView.Error -> String
    }
  }

info : InfoModel msg -> Html msg
info model =
  case model.view of
    Nothing ->
      case model.get |> HttpView.state of
        HttpView.Ready (Just error) ->
          H.section [ "loading" |> A.class ]
            [ H.section [] [ error |> model.i18n.http |> Content.badge ["is-danger"] ] ]
        _ ->
          H.section [ "loading" |> A.class ]
            [ H.section [] [ Icon.spinner ] ]
    Just view ->
      H.section []
        [ H.form [ model.msg.put |> E.onSubmit ]
          [ H.h2 [] [ "info" |> model.i18n.title |> H.text ]
          , H.table []
            [ H.tbody [] <| List.concat
              [ case (view.isStatic,view.form.name) of
                (True,(name,form,opts)) ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.p [] [ form.last |> H.text ]
                      ]
                    ]
                  ]
                (False,(name,form,opts)) ->
                  [ H.tr ( opts.errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.text []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , form |> Conflict.state |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , opts.errors |> Input.errors model.i18n.error
                      ]
                    ]
                  ]
              , case (view.isStatic,view.form.memo) of
                (True,(name,form,opts)) ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.p [ "text-pre-wrap" |> A.class ] [ form.last |> H.text ]
                      ]
                    ]
                  ]
                (False,(name,form,opts)) ->
                  [ H.tr ( opts.errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.textarea []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , form |> Conflict.state |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , opts.errors |> Input.errors model.i18n.error
                      ]
                    ]
                  ]
              , case (view.isStatic,view.form.age) of
                (True,(name,form,opts)) ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.p [] [ form.last |> H.text ]
                      ]
                    ]
                  ]
                (False,(name,form,opts)) ->
                  [ H.tr ( opts.errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.number []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , form |> Conflict.state |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , opts.errors |> Input.errors model.i18n.error
                      ]
                    ]
                  ]
              , case (view.isStatic,view.form.email) of
                (True,(name,form,opts)) ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.p [] [ form.last |> H.text ]
                      ]
                    ]
                  ]
                (False,(name,form,opts)) ->
                  [ H.tr ( opts.errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.email []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , form |> Conflict.state |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , opts.errors |> Input.errors model.i18n.error
                      ]
                    ]
                  ]
              , case (view.isStatic,view.form.tel) of
                (True,(name,form,opts)) ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.p [] [ form.last |> H.text ]
                      ]
                    ]
                  ]
                (False,(name,form,opts)) ->
                  [ H.tr ( opts.errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.tel []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , form |> Conflict.state |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , opts.errors |> Input.errors model.i18n.error
                      ]
                    ]
                  ]
              ]
            ]
          , H.footer [] <|
            if view.isStatic
              then
                [ "edit" |> model.i18n.form |> Button.edit model.msg.edit
                ]
              else
                case model.put |> HttpView.state of
                  HttpView.Connecting progress ->
                    [ "saving" |> model.i18n.form |> Button.connecting
                    , progress |> Http.progress
                    ]
                  HttpView.Ready response ->
                    [ case model.get |> HttpView.state of
                      HttpView.Connecting _ -> Icon.spinner
                      HttpView.Ready _ ->
                        case view.state of
                          Edit.Same -> "" |> H.text
                          Edit.HasError    -> "has-error" |> model.i18n.form |> Button.error
                          Edit.HasModified -> "save"      |> model.i18n.form |> Button.save
                    , " " |> H.text
                    , "cancel" |> model.i18n.form |> Button.cancel model.msg.cancel
                    , response |> Http.error model.i18n.http
                    ]
          ]
        ]

pairs : Info.Fields -> List ( String, String )
pairs fields =
  [ fields.name  |> Field.id_value
  , fields.memo  |> Field.id_value
  , fields.age   |> Field.id_value
  , fields.email |> Field.id_value
  , fields.tel   |> Field.id_value
  ]
