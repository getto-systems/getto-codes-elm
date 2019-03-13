module GettoUpload.App.Data.Upload.Edit.Unregister.Html exposing
  ( unregister
  , dialog
  )
import GettoUpload.App.Data.Upload.Edit.Data.View as Data
import GettoUpload.View.Html.Button as Button
import GettoUpload.View.Html.Http as Http
import GettoUpload.View.Http as HttpView

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias CompleteModel msg =
  { get : HttpView.Model Data.Response
  , msg :
    { confirm : msg
    }
  , i18n :
    { title : String -> String
    , form  : String -> String
    }
  }

unregister : CompleteModel msg -> Html msg
unregister model =
  case model.get |> HttpView.response of
    Nothing -> "" |> H.text
    Just res ->
      let
        body = res |> HttpView.body
      in
        H.section []
          [ H.form [ model.msg.confirm |> E.onSubmit ]
            [ H.h2 [] [ "unregister" |> model.i18n.title |> H.text ]
            , "delete" |> model.i18n.form |> Button.delete
            ]
          ]


type alias DialogModel msg =
  { isConfirm  : Bool
  , unregister : HttpView.State
  , msg :
    { unregister : msg
    , cancel     : msg
    }
  , i18n :
    { title   : String -> String
    , message : String -> String
    , form    : String -> String
    , http    : HttpView.Error -> String
    }
  }

dialog : DialogModel msg -> Html msg
dialog model =
  if model.isConfirm
    then
      H.section [ "dialog" |> A.class ] <|
        [ H.form [ model.msg.unregister |> E.onSubmit ]
          [ H.h3 [] [ "unregister" |> model.i18n.title   |> H.text ]
          , H.p  [] [ "unregister" |> model.i18n.message |> H.text ]
          , H.footer [ "is-center" |> A.class ] <|
            case model.unregister of
              HttpView.Connecting _ ->
                [ "deleting" |> model.i18n.form |> Button.connecting
                ]
              HttpView.Ready response ->
                [ "delete" |> model.i18n.form |> Button.delete
                , "cancel" |> model.i18n.form |> Button.cancel model.msg.cancel
                , H.p [] [ response |> Http.error model.i18n.http ]
                ]
          ]
        ]

    else "" |> H.text
