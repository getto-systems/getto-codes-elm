module GettoUpload.App.Data.Upload.New.Register.Html exposing
  ( register
  , pairs
  )
import GettoUpload.App.Data.Upload.New.Register.View as View
import GettoUpload.View.Html as Html
import GettoUpload.View.Html.Button as Button
import GettoUpload.View.Html.Input as Input
import GettoUpload.View.Html.Http as Http
import GettoUpload.View.Icon as Icon
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form

import File exposing ( File )

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias RegisterModel msg =
  { view   : View.View
  , upload : HttpView.Model View.Response
  , options :
    { gender  : List ( String, String )
    , quality : List ( String, String )
    , roles   : List ( String, String )
    }
  , msg :
    { upload : msg
    , input  : View.Prop String -> String -> msg
    , toggle : View.Prop (Set String) -> String -> msg
    , change : msg
    , select : View.Prop (List File) -> msg
    }
  , i18n :
    { title : String -> String
    , field : String -> String
    , error : String -> String
    , form  : String -> String
    , http  : HttpView.Error -> String
    }
  }

register : RegisterModel msg -> Html msg
register model =
  H.section []
    [ H.form [ model.msg.upload |> E.onSubmit ]
      [ H.h2 [] [ "register" |> model.i18n.title |> H.text ]
      , H.table []
        [ H.tbody [] <| List.concat
          [ case model.view.form.name of
            (name,form,opts) ->
              [ H.tr ( opts.errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.text [] (model.msg.input form.prop) model.msg.change
                    ]
                  , opts.errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.view.form.text of
            (name,form,opts) ->
              [ H.tr ( opts.errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ "select-file" |> model.i18n.form |> Button.select (model.msg.select form.prop)
                    , form.field |> Field.value |> Input.files
                    ]
                  , opts.errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.view.form.memo of
            (name,form,opts) ->
              [ H.tr ( opts.errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.textarea [] (model.msg.input form.prop) model.msg.change
                    ]
                  , opts.errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.view.form.age of
            (name,form,opts) ->
              [ H.tr ( opts.errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.number [] (model.msg.input form.prop) model.msg.change
                    ]
                  , opts.errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.view.form.email of
            (name,form,opts) ->
              [ H.tr ( opts.errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.email [] (model.msg.input form.prop) model.msg.change
                    ]
                  , opts.errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.view.form.tel of
            (name,form,opts) ->
              [ H.tr ( opts.errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.tel [] (model.msg.input form.prop) model.msg.change
                    ]
                  , opts.errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.view.form.birthday of
            (name,form,opts) ->
              [ H.tr ( opts.errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.date [] (model.msg.input form.prop) model.msg.change
                    ]
                  , opts.errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.view.form.start_at of
            (name,form,opts) ->
              [ H.tr ( opts.errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.time [] (model.msg.input form.prop) model.msg.change
                    ]
                  , opts.errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.view.form.gender of
            (name,form,opts) ->
              [ H.tr ( opts.errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.select model.options.gender [] (model.msg.input form.prop) model.msg.change
                    ]
                  , opts.errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.view.form.quality of
            (name,form,opts) ->
              [ H.tr ( opts.errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.radio model.options.quality [] (model.msg.input form.prop) model.msg.change
                    ]
                  , opts.errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.view.form.roles of
            (name,form,opts) ->
              [ H.tr ( opts.errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.checkbox model.options.roles [] (model.msg.toggle form.prop) model.msg.change
                    ]
                  , opts.errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          ]
        ]
      , H.footer [] <|
        case model.upload |> HttpView.state of
          HttpView.Connecting progress ->
            [ "uploading" |> model.i18n.form |> Button.connecting
            , progress |> Http.progress
            ]
          HttpView.Ready error ->
            if model.view.hasError
              then
                [ "has-error" |> model.i18n.form |> Button.error
                ]
              else
                [ "upload" |> model.i18n.form |> Button.save
                , error |> Http.error model.i18n.http
                ]
      ]
    ]

pairs : View.Form -> List ( String, String )
pairs form =
  [ form.name     |> Field.id_value
  , form.memo     |> Field.id_value
  , form.age      |> Field.id_value
  , form.email    |> Field.id_value
  , form.tel      |> Field.id_value
  , form.birthday |> Field.id_value
  , form.start_at |> Field.id_value
  ]
