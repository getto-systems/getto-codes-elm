module GettoCodes.App.Data.Upload.ListEdit.Detail.Html exposing
  ( detail
  , pairs
  )
import GettoCodes.App.Data.Upload.ListEdit.Detail.View as Detail
import GettoCodes.View.Html as Html
import GettoCodes.View.Html.Button as Button
import GettoCodes.View.Html.Input as Input
import GettoCodes.View.Html.Http as Http
import GettoCodes.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Edit as Edit
import Getto.Field.Conflict as Conflict

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias DetailModel msg =
  { view : Maybe Detail.View
  , options :
    { gender : List ( String, String )
    }
  , msg :
    { put     : msg
    , change  : msg
    , cancel  : msg
    , input   : Detail.Prop String -> String -> msg
    , resolve : Detail.Prop String -> Conflict.Resolve String -> msg
    }
  , i18n :
    { title : String -> String
    , field : String -> String
    , error : String -> String
    , form  : String -> String
    , http  : HttpView.Error -> String
    }
  }

detail : DetailModel msg -> Html msg
detail model =
  case model.view of
    Nothing -> "" |> H.text
    Just v ->
      H.section [ "dialog" |> A.class ]
        [ H.section [ "edit" |> A.class ]
          [ H.section []
            [ H.form [ model.msg.put |> E.onSubmit ]
              [ H.h2 [] [ "info" |> model.i18n.title |> H.text ]
              , H.table []
                [ H.tbody [] <| List.concat
                  [ case (v.view.isStatic,v.view.form.name) of
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
                  , case (v.view.isStatic,v.view.form.gender) of
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
                          [ [ form.field |> Input.select model.options.gender []
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
                case v.put |> HttpView.state of
                  HttpView.Connecting progress ->
                    [ "saving" |> model.i18n.form |> Button.connecting
                    , progress |> Http.progress
                    ]
                  HttpView.Ready response ->
                    [ case v.get |> HttpView.state of
                        HttpView.Connecting _ -> Html.spinner
                        HttpView.Ready _ ->
                          case v.view.state of
                            Edit.Same -> "" |> H.text
                            Edit.HasError    -> "has-error" |> model.i18n.form |> Button.error
                            Edit.HasModified -> "save"      |> model.i18n.form |> Button.save
                    , " " |> H.text
                    , "cancel" |> model.i18n.form |> Button.cancel model.msg.cancel
                    , response |> Http.error model.i18n.http
                    ]
              ]
            ]
          ]
        ]

pairs : Detail.Fields -> List ( String, String )
pairs fields =
  [ fields.name |> Field.id_value
  ]
