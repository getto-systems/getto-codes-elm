module GettoUpload.App.Upload.Edit.Detail.Html exposing
  ( detail
  )
import GettoUpload.App.Upload.Edit.Detail.View as View
import GettoUpload.View.Html as Html
import GettoUpload.View.Html.Button as Button
import GettoUpload.View.Html.Input as Input
import GettoUpload.View.Html.Http as Http
import GettoUpload.View.Icon as Icon
import GettoUpload.View.Http as HttpView

import Getto.Field.Edit exposing ( State(..) )
import Getto.Field.Conflict as Conflict

import File exposing ( File )

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias DetailModel msg =
  { title : String
  , view  : View.View
  , put   : HttpView.State
  , options :
    { gender  : List ( String, String )
    , quality : List ( String, String )
    , roles   : List ( String, String )
    }
  , msg :
    { put        : msg
    , input      : View.Prop String -> String -> msg
    , toggle     : View.Prop (Set String) -> String -> msg
    , change     : msg
    , edit       : msg
    , static     : msg
    , resolve    : View.Prop String -> Conflict.Resolve String -> msg
    , resolveSet : View.Prop (Set String) -> Conflict.Resolve (Set String) -> msg
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
  case model.view.response of
    Nothing ->
      case model.view.state of
        HttpView.Ready (Just error) ->
          H.section [ "loading" |> A.class ]
            [ H.section [] [ error |> model.i18n.http |> Html.badge ["is-danger"] ] ]
        _ ->
          H.section [ "loading" |> A.class ]
            [ H.section [] [ Html.spinner ] ]
    Just res ->
      let
        header = res |> HttpView.header
        body   = res |> HttpView.body
      in
        H.section []
          [ H.form [ model.msg.put |> E.onSubmit ]
            [ H.h2 [] [ model.title |> model.i18n.title |> H.text ]
            , H.table []
              [ H.tbody [] <| List.concat
                [ case model.view.form.birthday of
                  Static name ->
                    [ H.tr []
                      [ H.th [] [ name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [] [ body.detail.birthday |> H.text ]
                        ]
                      ]
                    ]
                  Edit name form conflict errors ->
                    [ H.tr ( errors |> Input.isError )
                      [ H.th [] [ name |> model.i18n.field |> H.text ]
                      , H.td [] <| List.concat
                        [ [ form.field |> Input.date [] (model.msg.input form.prop) model.msg.change
                          ]
                        , conflict |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                        , errors   |> Input.errors   model.i18n.error
                        ]
                      ]
                    ]
                , case model.view.form.start_at of
                  Static name ->
                    [ H.tr []
                      [ H.th [] [ name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [] [ body.detail.start_at |> H.text ]
                        ]
                      ]
                    ]
                  Edit name form conflict errors ->
                    [ H.tr ( errors |> Input.isError )
                      [ H.th [] [ name |> model.i18n.field |> H.text ]
                      , H.td [] <| List.concat
                        [ [ form.field |> Input.time [] (model.msg.input form.prop) model.msg.change
                          ]
                        , conflict |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                        , errors   |> Input.errors   model.i18n.error
                        ]
                      ]
                    ]
                , case model.view.form.gender of
                  Static name ->
                    [ H.tr []
                      [ H.th [] [ name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [] [ body.detail.gender |> H.text ]
                        ]
                      ]
                    ]
                  Edit name form conflict errors ->
                    [ H.tr ( errors |> Input.isError )
                      [ H.th [] [ name |> model.i18n.field |> H.text ]
                      , H.td [] <| List.concat
                        [ [ form.field |> Input.select model.options.gender [] (model.msg.input form.prop) model.msg.change
                          ]
                        , conflict |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                        , errors   |> Input.errors   model.i18n.error
                        ]
                      ]
                    ]
                , case model.view.form.quality of
                  Static name ->
                    [ H.tr []
                      [ H.th [] [ name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.p [] [ body.detail.quality |> H.text ]
                        ]
                      ]
                    ]
                  Edit name form conflict errors ->
                    [ H.tr ( errors |> Input.isError )
                      [ H.th [] [ name |> model.i18n.field |> H.text ]
                      , H.td [] <| List.concat
                        [ [ form.field |> Input.radio model.options.quality [] (model.msg.input form.prop) model.msg.change
                          ]
                        , conflict |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                        , errors   |> Input.errors   model.i18n.error
                        ]
                      ]
                    ]
                , case model.view.form.roles of
                  Static name ->
                    [ H.tr []
                      [ H.th [] [ name |> model.i18n.field |> H.text ]
                      , H.td []
                        [ H.ul []
                          ( body.detail.roles |> Set.toList |> List.map
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
                        [ [ form.field |> Input.checkbox model.options.roles [] (model.msg.toggle form.prop) model.msg.change
                          ]
                        , conflict |> Input.conflict model.i18n.form (model.msg.resolveSet form.prop)
                        , errors   |> Input.errors   model.i18n.error
                        ]
                      ]
                    ]
                ]
              ]
            , H.footer [] <|
              if model.view.isStatic
                then
                  [ "edit" |> model.i18n.form |> Button.edit model.msg.edit
                  ]
                else
                  case model.put of
                    HttpView.Connecting progress ->
                      [ "saving" |> model.i18n.form |> Button.connecting
                      , progress |> Http.progress
                      ]
                    HttpView.Ready response ->
                      [ if model.view.hasError
                        then "has-error" |> model.i18n.form |> Button.error
                        else
                          case model.view.state of
                            HttpView.Connecting _ -> Html.spinner
                            HttpView.Ready _ ->
                              "save" |> model.i18n.form |> Button.save
                      , " " |> H.text
                      , "cancel" |> model.i18n.form |> Button.cancel model.msg.static
                      , response |> Http.error model.i18n.http
                      ]
            ]
          ]
