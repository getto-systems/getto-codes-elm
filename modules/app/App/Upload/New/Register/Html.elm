module GettoUpload.App.Upload.New.Register.Html exposing
  ( register
  )
import GettoUpload.App.Upload.New.Register.View as View
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
  { title : String
  , form  : View.View
  , http  : HttpView.Model View.ResponseHeader View.ResponseBody
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
      [ H.h2 [] [ model.title |> model.i18n.title |> H.text ]
      , H.table []
        [ H.tbody [] <| List.concat
          [ case model.form |> View.name of
            (name,form,errors) ->
              [ H.tr ( errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.text [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.text of
            (name,form,errors) ->
              [ H.tr ( errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ "select-file" |> model.i18n.form |> Button.select (model.msg.select form.prop)
                    , form.field |> Field.value |> Input.files
                    ]
                  , errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.memo of
            (name,form,errors) ->
              [ H.tr ( errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.textarea [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.age of
            (name,form,errors) ->
              [ H.tr ( errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.number [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.email of
            (name,form,errors) ->
              [ H.tr ( errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.email [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.tel of
            (name,form,errors) ->
              [ H.tr ( errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.tel [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.birthday of
            (name,form,errors) ->
              [ H.tr ( errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.date [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.start_at of
            (name,form,errors) ->
              [ H.tr ( errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.time [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.gender of
            (name,form,errors) ->
              [ H.tr ( errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.select model.options.gender [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.quality of
            (name,form,errors) ->
              [ H.tr ( errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.radio model.options.quality [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.roles of
            (name,form,errors) ->
              [ H.tr ( errors |> Input.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> Input.checkbox model.options.roles [] (model.msg.toggle form.prop) model.msg.change
                    ]
                  , errors |> Input.errors model.i18n.error
                  ]
                ]
              ]
          ]
        ]
      , H.footer [] <|
        case model.http |> HttpView.state of
          HttpView.Connecting progress ->
            [ "uploading" |> model.i18n.form |> Button.connecting
            , progress |> Http.progress
            ]
          HttpView.Ready error ->
            if model.form |> View.hasError
              then
                [ "has-error" |> model.i18n.form |> Button.error
                ]
              else
                [ "upload" |> model.i18n.form |> Button.save
                , error |> Http.error model.i18n.http
                ]
      ]
    ]
