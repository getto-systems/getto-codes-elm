module GettoUpload.App.Data.Upload.Edit.Info.Html exposing
  ( info
  , pairs
  )
import GettoUpload.App.Data.Upload.Edit.Data.View as Data
import GettoUpload.App.Data.Upload.Edit.Info.View as Info
import GettoUpload.View.Html as Html
import GettoUpload.View.Html.Button as Button
import GettoUpload.View.Html.Input as Input
import GettoUpload.View.Html.Http as Http
import GettoUpload.View.Icon as Icon
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Edit exposing ( State(..), AggregateState(..) )
import Getto.Field.Conflict as Conflict

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias InfoModel msg =
  { view : Info.View
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
  case model.get |> model.view of
    Nothing ->
      case model.get |> HttpView.state of
        HttpView.Ready (Just error) ->
          H.section [ "loading" |> A.class ]
            [ H.section [] [ error |> model.i18n.http |> Html.badge ["is-danger"] ] ]
        _ ->
          H.section [ "loading" |> A.class ]
            [ H.section [] [ Html.spinner ] ]
    Just view ->
      H.section []
        [ H.form [ model.msg.put |> E.onSubmit ]
          [ H.h2 [] [ "info" |> model.i18n.title |> H.text ]
          , H.table []
            [ H.tbody [] <| List.concat
              [ case view.form.name of
                Static name value ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.p [] [ value |> H.text ]
                      ]
                    ]
                  ]
                Edit name form conflict errors ->
                  [ H.tr ( errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.text []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , conflict |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , errors   |> Input.errors   model.i18n.error
                      ]
                    ]
                  ]
              , case view.form.memo of
                Static name value ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.p [ "text-pre-wrap" |> A.class ] [ value |> H.text ]
                      ]
                    ]
                  ]
                Edit name form conflict errors ->
                  [ H.tr ( errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.textarea []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , conflict |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , errors   |> Input.errors   model.i18n.error
                      ]
                    ]
                  ]
              , case view.form.age of
                Static name value ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.p [] [ value |> H.text ]
                      ]
                    ]
                  ]
                Edit name form conflict errors ->
                  [ H.tr ( errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.number []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , conflict |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , errors   |> Input.errors   model.i18n.error
                      ]
                    ]
                  ]
              , case view.form.email of
                Static name value ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.p [] [ value |> H.text ]
                      ]
                    ]
                  ]
                Edit name form conflict errors ->
                  [ H.tr ( errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.email []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , conflict |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , errors   |> Input.errors   model.i18n.error
                      ]
                    ]
                  ]
              , case view.form.tel of
                Static name value ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.p [] [ value |> H.text ]
                      ]
                    ]
                  ]
                Edit name form conflict errors ->
                  [ H.tr ( errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.tel []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , conflict |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , errors   |> Input.errors   model.i18n.error
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
                      HttpView.Connecting _ -> Html.spinner
                      HttpView.Ready _ ->
                        case view.state of
                          Same -> "" |> H.text
                          HasError    -> "has-error" |> model.i18n.form |> Button.error
                          HasModified -> "save"      |> model.i18n.form |> Button.save
                    , " " |> H.text
                    , "cancel" |> model.i18n.form |> Button.cancel model.msg.cancel
                    , response |> Http.error model.i18n.http
                    ]
          ]
        ]

pairs : Info.Fields -> List ( String, String )
pairs fields =
  [ fields.name  |> Field.pair
  , fields.memo  |> Field.pair
  , fields.age   |> Field.pair
  , fields.email |> Field.pair
  , fields.tel   |> Field.pair
  ]
