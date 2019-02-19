module GettoUpload.App.Upload.List.Search.Html exposing
  ( search
  )
import GettoUpload.App.Upload.List.Search.View as View
import GettoUpload.View.Html as Html
import GettoUpload.View.Html.Button as ButtonHtml
import GettoUpload.View.Html.Field as FieldHtml
import GettoUpload.View.Html.Http as HttpHtml
import GettoUpload.View.Icon as Icon
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias SearchModel msg =
  { form  : View.View
  , http  : HttpView.Model View.ResponseHeader View.ResponseBody
  , options :
    { gender : List ( String, String )
    , roles  : List ( String, String )
    }
  , msg :
    { search : msg
    , input  : Form.Prop View.Form String -> String -> msg
    , check  : Form.Prop View.Form (Set String) -> String -> msg
    , change : msg
    }
  , i18n :
    { field : String -> String
    , form  : String -> String
    , http  : HttpView.Error -> String
    }
  }

search : SearchModel msg -> Html msg
search model =
  H.section [ "search" |> A.class ]
    [ H.form [ model.msg.search |> E.onSubmit ]
      [ H.section []
        [ H.div []
          [ H.table []
            [ H.tbody [] <| List.concat
              [ case model.form |> View.name of
                (name,present,form) ->
                  [ H.tr ( present |> FieldHtml.isPresent )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ form.field |> FieldHtml.text [] (model.msg.input form.prop) model.msg.change
                      ]
                    ]
                  ]
              , case model.form |> View.email of
                (name,present,form) ->
                  [ H.tr ( present |> FieldHtml.isPresent )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ form.field |> FieldHtml.text [] (model.msg.input form.prop) model.msg.change
                      ]
                    ]
                  ]
              , case model.form |> View.tel of
                (name,present,form) ->
                  [ H.tr ( present |> FieldHtml.isPresent )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ form.field |> FieldHtml.tel [] (model.msg.input form.prop) model.msg.change
                      ]
                    ]
                  ]
              ]
            ]
          ]
        , H.div []
          [ H.table []
            [ H.tbody [] <| List.concat
              [ case model.form |> View.age of
                (name,present,form) ->
                  [ H.tr ( present |> FieldHtml.isPresent )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ form.gteq.field |> FieldHtml.number [] (model.msg.input form.gteq.prop) model.msg.change
                      , " ～ " |> H.text
                      , form.lteq.field |> FieldHtml.number [] (model.msg.input form.lteq.prop) model.msg.change
                      ]
                    ]
                  ]
              , case model.form |> View.birthday of
                (name,present,form) ->
                  [ H.tr ( present |> FieldHtml.isPresent )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ form.gteq.field |> FieldHtml.date [] (model.msg.input form.gteq.prop) model.msg.change
                      , " ～ " |> H.text
                      , form.lteq.field |> FieldHtml.date [] (model.msg.input form.lteq.prop) model.msg.change
                      ]
                    ]
                  ]
              , case model.form |> View.start_at of
                (name,present,form) ->
                  [ H.tr ( present |> FieldHtml.isPresent )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ form.gteq.field |> FieldHtml.time [] (model.msg.input form.gteq.prop) model.msg.change
                      , " ～ " |> H.text
                      , form.lteq.field |> FieldHtml.time [] (model.msg.input form.lteq.prop) model.msg.change
                      ]
                    ]
                  ]
              ]
            ]
          ]
        , H.div []
          [ H.table []
            [ H.tbody [] <| List.concat
              [ case model.form |> View.gender of
                (name,present,form) ->
                  [ H.tr ( present |> FieldHtml.isPresent )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ form.field |> FieldHtml.select model.options.gender [] (model.msg.input form.prop) model.msg.change
                      ]
                    ]
                  ]
              , case model.form |> View.roles of
                (name,present,form) ->
                  [ H.tr ( present |> FieldHtml.isPresent )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ form.field |> FieldHtml.checkbox model.options.roles [] (model.msg.check form.prop) model.msg.change
                      ]
                    ]
                  ]
              ]
            ]
          ]
        ]
      , H.footer [] <|
        case model.http |> HttpView.state of
          HttpView.Connecting progress ->
            [ "searching" |> model.i18n.form |> ButtonHtml.connecting
            , progress |> HttpHtml.progress
            ]
          HttpView.Ready error ->
            [ "search" |> model.i18n.form |> ButtonHtml.search
            , error |> HttpHtml.error model.i18n.http
            ]
      ]
    ]
