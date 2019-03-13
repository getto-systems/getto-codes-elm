module GettoUpload.App.Data.Upload.Edit.Complete.Html exposing
  ( state
  , dialog
  )
import GettoUpload.App.Data.Upload.Edit.Data.View as Data
import GettoUpload.View.Html as Html
import GettoUpload.View.Html.Button as Button
import GettoUpload.View.Html.Http as Http
import GettoUpload.View.Http as HttpView

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias CompleteModel msg =
  { get : HttpView.Model Data.Response
  , msg :
    { edit : msg
    }
  , i18n :
    { title : String -> String
    , form  : String -> String
    , field : String -> String
    , state : Data.State -> String
    }
  }

state : CompleteModel msg -> Html msg
state model =
  case model.get |> HttpView.response of
    Nothing -> "" |> H.text
    Just res ->
      let
        body = res |> HttpView.body
      in
        H.section []
          [ H.form []
            [ H.h2 [] [ "complete" |> model.i18n.title |> H.text ]
            , H.table []
              [ H.tbody []
                [ H.tr []
                  [ H.th [] [ "state" |> model.i18n.field |> H.text ]
                  , H.td []
                    [ body.state.state |> model.i18n.state |> Html.badge [ body.state.state |> stateClass ]
                    ]
                  ]
                ]
              ]
            , H.footer []
              [ "edit" |> model.i18n.form |> Button.edit model.msg.edit
              ]
            ]
          ]

stateClass st =
  case st of
    Data.Working  -> "is-warning"
    Data.Complete -> "is-complete"


type alias DialogModel msg =
  { get      : HttpView.Model Data.Response
  , isEdit   : Bool
  , complete : HttpView.State
  , work     : HttpView.State
  , msg :
    { complete : msg
    , work     : msg
    , cancel   : msg
    }
  , i18n :
    { title   : String -> String
    , message : String -> String
    , button  : String -> String
    , form    : String -> String
    , http    : HttpView.Error -> String
    }
  }

dialog : DialogModel msg -> Html msg
dialog model =
  case ( model.get |> HttpView.response, model.isEdit ) of
    ( Just res, True ) ->
      let
        body = res |> HttpView.body
      in
        H.section [ "dialog" |> A.class ] <|
          case body.state.state of
            Data.Working ->
              [ H.form [ model.msg.complete |> E.onSubmit ]
                [ H.h3 [] [ "to-complete" |> model.i18n.title   |> H.text ]
                , H.p  [] [ "to-complete" |> model.i18n.message |> H.text ]
                , H.footer [ "is-center" |> A.class ] <|
                  case model.complete of
                    HttpView.Connecting _ ->
                      [ "to-complete-connecting" |> model.i18n.button |> Button.connecting
                      ]
                    HttpView.Ready response ->
                      [ "to-complete" |> model.i18n.button |> Button.complete [ "is-warning" ]
                      , "cancel"      |> model.i18n.form   |> Button.cancel model.msg.cancel
                      , H.p [] [ response |> Http.error model.i18n.http ]
                      ]
                ]
              ]
            Data.Complete ->
              [ H.form [ model.msg.work |> E.onSubmit ]
                [ H.h3 [] [ "to-work" |> model.i18n.title   |> H.text ]
                , H.p  [] [ "to-work" |> model.i18n.message |> H.text ]
                , H.footer [ "is-center" |> A.class ] <|
                  case model.work of
                    HttpView.Connecting _ ->
                      [ "to-work-connecting" |> model.i18n.button |> Button.connecting
                      ]
                    HttpView.Ready response ->
                      [ "to-work" |> model.i18n.button |> Button.complete [ "is-complete" ]
                      , "cancel"  |> model.i18n.form   |> Button.cancel model.msg.cancel
                      , H.p [] [ response |> Http.error model.i18n.http ]
                      ]
                ]
              ]

    _ -> "" |> H.text
