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
import Getto.Field.View as FieldView

import File exposing ( File )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias RegisterModel header body msg =
  { title  : String
  , form : View.View
  , state  : HttpView.State header body
  , options :
    { gender : List ( String, String )
    }
  , msg :
    { upload : msg
    , input  : View.Prop String -> String -> msg
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

register : RegisterModel header body msg -> Html msg
register model =
  H.section []
    [ H.form []
      [ H.h2 [] [ model.title |> model.i18n.title |> H.text ]
      , H.table []
        [ H.tbody [] <| List.concat
          [ case model.form |> View.name of
            (field,prop) ->
              [ H.tr ( field |> FieldHtml.isError )
                [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ field |> FieldHtml.text [] (model.msg.input prop) model.msg.change
                    ]
                  , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.text of
            (field,prop) ->
              [ H.tr ( field |> FieldHtml.isError )
                [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ "select-file" |> model.i18n.form |> ButtonHtml.select (model.msg.select prop)
                    , field |> FieldView.value |> FieldHtml.files
                    ]
                  , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.memo of
            (field,prop) ->
              [ H.tr ( field |> FieldHtml.isError )
                [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ field |> FieldHtml.textarea [] (model.msg.input prop) model.msg.change
                    ]
                  , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.age of
            (field,prop) ->
              [ H.tr ( field |> FieldHtml.isError )
                [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ field |> FieldHtml.number [] (model.msg.input prop) model.msg.change
                    ]
                  , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.email of
            (field,prop) ->
              [ H.tr ( field |> FieldHtml.isError )
                [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ field |> FieldHtml.email [] (model.msg.input prop) model.msg.change
                    ]
                  , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.tel of
            (field,prop) ->
              [ H.tr ( field |> FieldHtml.isError )
                [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ field |> FieldHtml.tel [] (model.msg.input prop) model.msg.change
                    ]
                  , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.birthday of
            (field,prop) ->
              [ H.tr ( field |> FieldHtml.isError )
                [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ field |> FieldHtml.date [] (model.msg.input prop) model.msg.change
                    ]
                  , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.start_at of
            (field,prop) ->
              [ H.tr ( field |> FieldHtml.isError )
                [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ field |> FieldHtml.time [] (model.msg.input prop) model.msg.change
                    ]
                  , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          , case model.form |> View.gender of
            (field,prop) ->
              [ H.tr ( field |> FieldHtml.isError )
                [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                , H.td [] <| List.concat
                  [ [ field |> FieldHtml.select model.options.gender [] (model.msg.input prop) model.msg.change
                    ]
                  , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                  ]
                ]
              ]
          ]
        ]
      , H.footer [] <|
        case model.state of
          HttpView.Connecting progress ->
            [ "uploading" |> model.i18n.form |> ButtonHtml.connecting
            , progress |> HttpHtml.progress
            ]
          HttpView.Ready response ->
            if model.form |> View.hasError
              then
                [ "has-error" |> model.i18n.form |> ButtonHtml.error
                ]
              else
                [ "upload" |> model.i18n.form |> ButtonHtml.save model.msg.upload
                , response |> HttpHtml.error model.i18n.http
                ]
      ]
    ]
