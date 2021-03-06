module GettoCodes.Html.Button exposing
  ( error
  , connecting
  , select
  , overwrite
  , revert
  , edit
  , cancel
  , save
  , delete
  , logout
  , search
  , complete
  )
import GettoCodes.View.Icon as IconView
import GettoCodes.Html.Icon as Icon

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

error : String -> Html msg
error text =
  button [ "is-error" |> A.class ]
    [ text |> H.text ]

connecting : String -> Html msg
connecting text =
  button [ "is-connecting" |> A.class ]
    [ Icon.spinner
    , " " |> H.text
    , text |> H.text
    ]

select : msg -> String -> Html msg
select msg text =
  button [ msg |> E.onClick ]
    [ text |> H.text ]

overwrite : msg -> String -> Html msg
overwrite msg text =
  button [ "is-save" |> A.class, msg |> E.onClick ]
    [ text |> H.text ]

revert : msg -> String -> Html msg
revert msg text =
  button [ "is-cancel" |> A.class, msg |> E.onClick ]
    [ text |> H.text ]

edit : msg -> String -> Html msg
edit msg text =
  button [ "is-edit" |> A.class, msg |> E.onClick ]
    [ text |> H.text ]

cancel : msg -> String -> Html msg
cancel msg text =
  button [ "is-cancel" |> A.class, msg |> E.onClick ]
    [ text |> H.text ]

button : List (H.Attribute msg) -> List (Html msg) -> Html msg
button attr =
  H.button ( ("button" |> A.type_) :: attr )


save : String -> Html msg
save text =
  submit [ "is-save" |> A.class ]
    [ text |> H.text ]

delete : String -> Html msg
delete text =
  submit [ "is-delete" |> A.class ]
    [ text |> H.text ]

logout : String -> Html msg
logout text =
  submit [ "is-delete" |> A.class ]
    [ IconView.fas "sign-out-alt" |> Icon.toHtml []
    , " " |> H.text
    , text |> H.text
    ]

search : String -> Html msg
search text =
  submit []
    [ text |> H.text ]

complete : List String -> String -> Html msg
complete class text =
  submit [ class |> String.join " " |> A.class ]
    [ text |> H.text ]

submit : List (H.Attribute msg) -> List (Html msg) -> Html msg
submit = H.button
