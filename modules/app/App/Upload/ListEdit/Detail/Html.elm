module GettoUpload.App.Upload.ListEdit.Detail.Html exposing
  ( detail
  , pairs
  )
import GettoUpload.App.Upload.ListEdit.Detail.View as Detail
import GettoUpload.View.Html as Html
import GettoUpload.View.Html.Button as Button
import GettoUpload.View.Html.Input as Input
import GettoUpload.View.Html.Http as Http
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Edit as Edit exposing ( State(..), AggregateState(..) )
import Getto.Field.Conflict as Conflict

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias DetailModel msg =
  { view : Detail.View
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
    Just view ->
      H.section [ "dialog" |> A.class ]
        [ H.section [ "edit" |> A.class ]
          [ H.section []
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
                  , case view.form.gender of
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
                          [ [ form.field |> Input.select model.options.gender []
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
                case view.put |> HttpView.state of
                  HttpView.Connecting progress ->
                    [ "saving" |> model.i18n.form |> Button.connecting
                    , progress |> Http.progress
                    ]
                  HttpView.Ready response ->
                    [ case view.get |> HttpView.state of
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
          ]
        ]

pairs : Detail.Fields -> List ( String, String )
pairs fields =
  [ fields.name |> Field.pair
  ]
