module GettoCodes.App.System.Profile.Dashboard.Html exposing
  ( logout
  )
import GettoCodes.Html.Button as Button

import Html as H exposing ( Html )
import Html.Events as E


type alias LogoutModel msg =
  { msg :
    { logout : msg
    }
  , i18n :
    { button : String -> String
    }
  }

logout : LogoutModel msg -> Html msg
logout model =
  H.section []
    [ H.p []
      [ H.form [ model.msg.logout |> E.onSubmit ]
        [ "logout" |> model.i18n.button |> Button.logout
        ]
      ]
    ]
