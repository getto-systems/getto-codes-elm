module GettoUpload.App.Upload.Edit.Info.Html exposing
  ( info
  )
import GettoUpload.App.Upload.Edit.Info.View as View
import GettoUpload.View.Html as Html
import GettoUpload.View.Html.Button as ButtonHtml
import GettoUpload.View.Html.Field as FieldHtml
import GettoUpload.View.Html.Http as HttpHtml
import GettoUpload.View.Icon as Icon
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.View as FieldView

import File exposing ( File )
import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias InfoModel msg =
  { title  : String
  , form : View.View
  , get  : HttpView.State View.ResponseHeader View.ResponseBody
  , put  : HttpView.State View.ResponseHeader View.ResponseBody
  , options :
    { gender  : List ( String, String )
    , quality : List ( String, String )
    , roles   : List ( String, String )
    }
  , msg :
    { put    : msg
    , input  : View.Prop String -> String -> msg
    , check  : View.Prop (Set String) -> String -> msg
    , change : msg
    , edit   : msg
    , static : msg
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
  case model.get of
    HttpView.Ready (Just (Ok get)) ->
      let
        res = HttpView.body <|
          case model.put of
            HttpView.Ready (Just (Ok put)) -> put
            _ -> get
      in
        H.section []
          [ H.form []
            [ H.h2 [] [ model.title |> model.i18n.title |> H.text ]
            , H.table []
              [ H.tbody [] <| List.concat
                [ case model.form |> View.name of
                  (View.Static,field,prop) ->
                    [ H.tr []
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [] [ res.info.name |> H.text ]
                        ]
                      ]
                    ]
                  (View.Edit,field,prop) ->
                    [ H.tr ( field |> FieldHtml.isError )
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td [] <| List.concat
                        [ [ field |> FieldHtml.text [] (model.msg.input prop) model.msg.change
                          ]
                        , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                        ]
                      ]
                    ]
                , case model.form |> View.memo of
                  (View.Static,field,prop) ->
                    [ H.tr []
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [ "text-pre-wrap" |> A.class ] [ res.info.memo |> H.text ]
                        ]
                      ]
                    ]
                  (View.Edit,field,prop) ->
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
                  (View.Static,field,prop) ->
                    [ H.tr []
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [] [ res.info.age |> String.fromInt |> H.text ]
                        ]
                      ]
                    ]
                  (View.Edit,field,prop) ->
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
                  (View.Static,field,prop) ->
                    [ H.tr []
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [] [ res.info.email |> H.text ]
                        ]
                      ]
                    ]
                  (View.Edit,field,prop) ->
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
                  (View.Static,field,prop) ->
                    [ H.tr []
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [] [ res.info.tel |> H.text ]
                        ]
                      ]
                    ]
                  (View.Edit,field,prop) ->
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
                  (View.Static,field,prop) ->
                    [ H.tr []
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [] [ res.detail.birthday |> H.text ]
                        ]
                      ]
                    ]
                  (View.Edit,field,prop) ->
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
                  (View.Static,field,prop) ->
                    [ H.tr ( field |> FieldHtml.isError )
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [] [ res.detail.start_at |> H.text ]
                        ]
                      ]
                    ]
                  (View.Edit,field,prop) ->
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
                  (View.Static,field,prop) ->
                    [ H.tr []
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [] [ res.detail.gender |> H.text ]
                        ]
                      ]
                    ]
                  (View.Edit,field,prop) ->
                    [ H.tr ( field |> FieldHtml.isError )
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td [] <| List.concat
                        [ [ field |> FieldHtml.select model.options.gender [] (model.msg.input prop) model.msg.change
                          ]
                        , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                        ]
                      ]
                    ]
                , case model.form |> View.quality of
                  (View.Static,field,prop) ->
                    [ H.tr []
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [] [ res.detail.quality |> H.text ]
                        ]
                      ]
                    ]
                  (View.Edit,field,prop) ->
                    [ H.tr ( field |> FieldHtml.isError )
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td [] <| List.concat
                        [ [ field |> FieldHtml.radio model.options.quality [] (model.msg.input prop) model.msg.change
                          ]
                        , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                        ]
                      ]
                    ]
                , case model.form |> View.roles of
                  (View.Static,field,prop) ->
                    [ H.tr []
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.ul []
                          ( res.detail.roles |> Set.toList |> List.map
                            (\role ->
                              H.li [] [ role |> H.text ]
                            )
                          )
                        ]
                      ]
                    ]
                  (View.Edit,field,prop) ->
                    [ H.tr ( field |> FieldHtml.isError )
                      [ H.th [] [ field |> FieldView.name |> model.i18n.field |> H.text ]
                      , H.td [] <| List.concat
                        [ [ field |> FieldHtml.checkbox model.options.roles [] (model.msg.check prop) model.msg.change
                          ]
                        , field |> FieldView.errors |> FieldHtml.errors model.i18n.error
                        ]
                      ]
                    ]
                ]
              ]
            , H.footer [] <|
              case model.form |> View.state of
                (View.Static,_) ->
                  [ "edit" |> model.i18n.form |> ButtonHtml.edit model.msg.edit
                  ]
                (View.Edit,hasError) ->
                  case model.put of
                    HttpView.Connecting progress ->
                      [ "saving" |> model.i18n.form |> ButtonHtml.connecting
                      , progress |> HttpHtml.progress
                      ]
                    HttpView.Ready response ->
                      [ if hasError
                        then "has-error" |> model.i18n.form |> ButtonHtml.error
                        else "save"      |> model.i18n.form |> ButtonHtml.save model.msg.put
                      , "cancel" |> model.i18n.form |> ButtonHtml.cancel model.msg.static
                      , response |> HttpHtml.error model.i18n.http
                      ]
            ]
          ]
    HttpView.Ready (Just (Err error)) ->
      H.section [ "loading" |> A.class ]
        [ H.section []
          [ error |> model.i18n.http |> Html.badge ["is-danger"]
          ]
        ]
    _ ->
      H.section [ "loading" |> A.class ]
        [ H.section []
          [ Icon.fas "spinner" |> Html.icon ["fa-pulse"]
          ]
        ]
