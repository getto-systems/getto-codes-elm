module GettoUpload.App.Upload.Edit.Detail.Html exposing
  ( detail
  , pairs
  )
import GettoUpload.App.Upload.Edit.Data.View   as Data
import GettoUpload.App.Upload.Edit.Detail.View as Detail
import GettoUpload.View.Html as Html
import GettoUpload.View.Html.Button as Button
import GettoUpload.View.Html.Input as Input
import GettoUpload.View.Html.Http as Http
import GettoUpload.View.Icon as Icon
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Edit exposing ( State(..) )
import Getto.Field.Conflict as Conflict

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias DetailModel msg =
  { view  : Detail.View
  , get   : HttpView.Model Data.Response
  , put   : HttpView.Model Detail.Response
  , options :
    { gender  : List ( String, String )
    , quality : List ( String, String )
    , roles   : List ( String, String )
    }
  , msg :
    { put        : msg
    , change     : msg
    , edit       : msg
    , cancel     : msg
    , input      : Detail.Prop String       -> String -> msg
    , toggle     : Detail.Prop (Set String) -> String -> msg
    , resolve    : Detail.Prop String       -> Conflict.Resolve String       -> msg
    , resolveSet : Detail.Prop (Set String) -> Conflict.Resolve (Set String) -> msg
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
  case model.get |> model.view of
    Nothing -> "" |> H.text
    Just view ->
      H.section []
        [ H.form [ model.msg.put |> E.onSubmit ]
          [ H.h2 [] [ "detail" |> model.i18n.title |> H.text ]
          , H.table []
            [ H.tbody [] <| List.concat
              [ case view.form.birthday of
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
                      [ [ form.field |> Input.date []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , conflict |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , errors   |> Input.errors   model.i18n.error
                      ]
                    ]
                  ]
              , case view.form.start_at of
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
                      [ [ form.field |> Input.time []
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
              , case view.form.quality of
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
                      [ [ form.field |> Input.radio model.options.quality []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , conflict |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , errors   |> Input.errors   model.i18n.error
                      ]
                    ]
                  ]
              , case view.form.roles of
                Static name value ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.ul []
                        ( value |> Set.toList |> List.map
                          (\role ->
                            H.li [] [ H.p [] [ role |> H.text ] ]
                          )
                        )
                      ]
                    ]
                  ]
                Edit name form conflict errors ->
                  [ H.tr ( errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.checkbox model.options.roles []
                          (model.msg.toggle form.prop) model.msg.change
                        ]
                      , conflict |> Input.conflict model.i18n.form (model.msg.resolveSet form.prop)
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
                    [ if view.hasError
                      then "has-error" |> model.i18n.form |> Button.error
                      else
                        case model.get |> HttpView.state of
                          HttpView.Connecting _ -> Html.spinner
                          HttpView.Ready _ ->
                            "save" |> model.i18n.form |> Button.save
                    , " " |> H.text
                    , "cancel" |> model.i18n.form |> Button.cancel model.msg.cancel
                    , response |> Http.error model.i18n.http
                    ]
          ]
        ]

pairs : Detail.Fields -> List ( String, String )
pairs fields =
  [ fields.birthday |> Field.pair
  , fields.start_at |> Field.pair
  ]
