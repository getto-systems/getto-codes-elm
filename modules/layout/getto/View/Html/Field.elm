module GettoUpload.View.Html.Field exposing
  ( isError
  , textSmall
  , text
  , textLarge
  , textXLarge
  , numberSmall
  , number
  , numberLarge
  , email
  , tel
  , date
  , time
  , textarea
  , files
  , errors
  , onChange
  )
import GettoUpload.View.Icon as Icon
import GettoUpload.View.Html as Html

import Getto.Field as Field
import Getto.Field.View as FieldView

import File exposing ( File )
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

isError : FieldView.Model a -> List (H.Attribute msg)
isError field =
  if field |> FieldView.isError
    then [ "form-error" |> A.class ]
    else []


textSmall  = "text" |> input "is-small"
text       = "text" |> input ""
textLarge  = "text" |> input "is-large"
textXLarge = "text" |> input "is-xlarge"

numberSmall  = "number" |> input "is-tiny"
number       = "number" |> input "is-small"
numberLarge  = "number" |> input ""

email = "email" |> input ""
tel   = "tel"   |> input ""
date  = "date"  |> input ""
time  = "time"  |> input ""

input : String -> String -> List (H.Attribute msg) -> (Field.Update -> String -> msg) -> FieldView.Model String -> Html msg
input class type_ attr msg field =
  L.lazy
    (\value ->
      H.input
        ( [ type_ |> A.type_
          , field |> FieldView.id |> A.id
          , class |> A.class
          , msg Field.Input  |> E.onInput
          , msg Field.Change |> onChange
          ]
          ++ attr
        )
        []
    )
    (field |> FieldView.value)

textarea       = textareaInput 8  ""
textareaLarge  = textareaInput 12 "is-large"
textareaXLarge = textareaInput 16 "is-xlarge"

textareaInput : Int -> String -> List (H.Attribute msg) -> (Field.Update -> String -> msg) -> FieldView.Model String -> Html msg
textareaInput rows class attr msg field =
  H.textarea
    ( [ class |> A.class
      , rows  |> A.rows
      , msg Field.Input  |> E.onInput
      , msg Field.Change |> onChange
      ]
      ++ attr
    )
    []


files : List File -> Html msg
files li =
  if li |> List.isEmpty
    then "" |> H.text
    else
      H.ul []
        (li |> List.map
          (\file ->
            H.li []
              [ Icon.far "file" |> Html.icon ["fa-fw"]
              , file |> File.name |> H.text
              ]
          )
        )

errors : (String -> String) -> List String -> List (Html msg)
errors i18n = List.map
  (\error ->
    H.p [ "help" |> A.class ] [ error |> i18n |> H.text ]
  )

onChange : (String -> msg) -> H.Attribute msg
onChange msg =
  E.targetValue |> Decode.map msg |> E.on "change"
