module GettoUpload.App.Upload.New.Register.Html exposing
  ( register
  )
import GettoUpload.App.Upload.New.Register.View as View
import GettoUpload.View.Html as Html
import GettoUpload.View.Html.Button as ButtonHtml
import GettoUpload.View.Html.Field as FieldHtml
import GettoUpload.View.Html.Http as HttpHtml
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
    , input  : Form.Prop View.Form String -> String -> msg
    , check  : Form.Prop View.Form (Set String) -> String -> msg
    , change : msg
    , select : Form.Prop View.Form (List File) -> msg
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
    [ H.form []
      [ H.h2 [] [ model.title |> model.i18n.title |> H.text ]
      , H.table []
        [ H.tbody [] <| List.concat
          [ case model.form |> View.name of
            (name,errors,form) ->
              [ H.tr ( errors |> FieldHtml.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> FieldHtml.text [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.text of
            (name,errors,form) ->
              [ H.tr ( errors |> FieldHtml.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ "select-file" |> model.i18n.form |> ButtonHtml.select (model.msg.select form.prop)
                    , form.field |> Field.value |> FieldHtml.files
                    ]
                  , errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.memo of
            (name,errors,form) ->
              [ H.tr ( errors |> FieldHtml.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> FieldHtml.textarea [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.age of
            (name,errors,form) ->
              [ H.tr ( errors |> FieldHtml.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> FieldHtml.number [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.email of
            (name,errors,form) ->
              [ H.tr ( errors |> FieldHtml.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> FieldHtml.email [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.tel of
            (name,errors,form) ->
              [ H.tr ( errors |> FieldHtml.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> FieldHtml.tel [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.birthday of
            (name,errors,form) ->
              [ H.tr ( errors |> FieldHtml.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> FieldHtml.date [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.start_at of
            (name,errors,form) ->
              [ H.tr ( errors |> FieldHtml.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> FieldHtml.time [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.gender of
            (name,errors,form) ->
              [ H.tr ( errors |> FieldHtml.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> FieldHtml.select model.options.gender [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.quality of
            (name,errors,form) ->
              [ H.tr ( errors |> FieldHtml.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> FieldHtml.radio model.options.quality [] (model.msg.input form.prop) model.msg.change
                    ]
                  , errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.roles of
            (name,errors,form) ->
              [ H.tr ( errors |> FieldHtml.isError )
                [ H.th [] [ name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ form.field |> FieldHtml.checkbox model.options.roles [] (model.msg.check form.prop) model.msg.change
                    ]
                  , errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          ]
        ]
      , H.footer [] <|
        case model.http |> HttpView.state of
          HttpView.Connecting progress ->
            [ "uploading" |> model.i18n.form |> ButtonHtml.connecting
            , progress |> HttpHtml.progress
            ]
          HttpView.Ready error ->
            if model.form |> View.hasError
              then
                [ "has-error" |> model.i18n.form |> ButtonHtml.error
                ]
              else
                [ "upload" |> model.i18n.form |> ButtonHtml.save model.msg.upload
                , error |> HttpHtml.error model.i18n.http
                ]
      ]
    ]
