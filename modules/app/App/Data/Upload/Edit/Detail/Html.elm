module GettoCodes.App.Data.Upload.Edit.Detail.Html exposing
  ( detail
  , pairs
  )
import GettoCodes.App.Data.Upload.Edit.Data.View   as Data
import GettoCodes.App.Data.Upload.Edit.Detail.View as Detail
import GettoCodes.View.Html as Html
import GettoCodes.View.Html.Button as Button
import GettoCodes.View.Html.Input as Input
import GettoCodes.View.Html.Http as Http
import GettoCodes.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Edit as Edit
import Getto.Field.Conflict as Conflict

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Events as E


type alias DetailModel msg =
  { view  : Maybe Detail.View
  , get   : HttpView.Model Data.Response
  , put   : HttpView.Model Detail.Response
  , options :
    { gender  : List ( String, String )
    , quality : List ( String, String )
    , role    : List ( String, String )
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
    { title   : String -> String
    , field   : String -> String
    , error   : String -> String
    , form    : String -> String
    , http    : HttpView.Error -> String
    , gender  : String -> String
    , quality : String -> String
    , role    : String -> String
    }
  }

detail : DetailModel msg -> Html msg
detail model =
  case model.view of
    Nothing -> "" |> H.text
    Just view ->
      H.section []
        [ H.form [ model.msg.put |> E.onSubmit ]
          [ H.h2 [] [ "detail" |> model.i18n.title |> H.text ]
          , H.table []
            [ H.tbody [] <| List.concat
              [ case (view.isStatic,view.form.birthday) of
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
                      [ [ form.field |> Input.date []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , form |> Conflict.state |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , opts.errors |> Input.errors model.i18n.error
                      ]
                    ]
                  ]
              , case (view.isStatic,view.form.start_at) of
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
                      [ [ form.field |> Input.time []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , form |> Conflict.state |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , opts.errors |> Input.errors model.i18n.error
                      ]
                    ]
                  ]
              , case (view.isStatic,view.form.gender) of
                (True,(name,form,opts)) ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.p [] [ form.last |> model.i18n.gender |> H.text ]
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
              , case (view.isStatic,view.form.quality) of
                (True,(name,form,opts)) ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.p [] [ form.last |> model.i18n.quality |> H.text ]
                      ]
                    ]
                  ]
                (False,(name,form,opts)) ->
                  [ H.tr ( opts.errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.radio model.options.quality []
                          (model.msg.input form.prop) model.msg.change
                        ]
                      , form |> Conflict.state |> Input.conflict model.i18n.form (model.msg.resolve form.prop)
                      , opts.errors |> Input.errors model.i18n.error
                      ]
                    ]
                  ]
              , case (view.isStatic,view.form.roles) of
                (True,(name,form,opts)) ->
                  [ H.tr []
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td []
                      [ H.ul []
                        ( form.last |> Set.toList |> List.map
                          (\role ->
                            H.li [] [ H.p [] [ role |> model.i18n.role |> H.text ] ]
                          )
                        )
                      ]
                    ]
                  ]
                (False,(name,form,opts)) ->
                  [ H.tr ( opts.errors |> Input.isError )
                    [ H.th [] [ name |> model.i18n.field |> H.text ]
                    , H.td [] <| List.concat
                      [ [ form.field |> Input.checkbox model.options.role []
                          (model.msg.toggle form.prop) model.msg.change
                        ]
                      , form |> Conflict.state |> Input.conflict model.i18n.form (model.msg.resolveSet form.prop)
                      , opts.errors |> Input.errors model.i18n.error
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
                    [ case model.get |> HttpView.state of
                      HttpView.Connecting _ -> Html.spinner
                      HttpView.Ready _ ->
                        case view.state of
                          Edit.Same -> "" |> H.text
                          Edit.HasError    -> "has-error" |> model.i18n.form |> Button.error
                          Edit.HasModified -> "save"      |> model.i18n.form |> Button.save
                    , " " |> H.text
                    , "cancel" |> model.i18n.form |> Button.cancel model.msg.cancel
                    , response |> Http.error model.i18n.http
                    ]
          ]
        ]

pairs : Detail.Fields -> List ( String, String )
pairs fields =
  [ fields.birthday |> Field.id_value
  , fields.start_at |> Field.id_value
  ]
