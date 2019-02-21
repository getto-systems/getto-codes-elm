module GettoUpload.View.Html.Input exposing
  ( Paging
  , isError
  , isPresent
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
  , select
  , radio
  , radioInline
  , checkbox
  , checkboxInline
  , checkboxBlock
  , files
  , errors
  , onChange
  , paging
  )
import GettoUpload.View.Icon as Icon
import GettoUpload.View.Html as Html

import Getto.Field as Field

import File exposing ( File )
import Set exposing ( Set )
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias Paging =
  { page : Int
  , max  : Int
  }

isPresent : Bool -> List (H.Attribute msg)
isPresent presence =
  if presence
    then [ "is-present" |> A.class ]
    else []

isError : List String -> List (H.Attribute msg)
isError err =
  if err |> List.isEmpty |> not
    then [ "form-error" |> A.class ]
    else []


textSmall  = "text" |> input ["is-small"]
text       = "text" |> input []
textLarge  = "text" |> input ["is-large"]
textXLarge = "text" |> input ["is-xlarge"]

numberSmall  = "number" |> input ["is-tiny"]
number       = "number" |> input ["is-small"]
numberLarge  = "number" |> input []

email = "email" |> input []
tel   = "tel"   |> input []
date  = "date"  |> input []
time  = "time"  |> input []

input : List String -> String -> List (H.Attribute msg) -> (String -> msg) -> msg -> Field.Model String -> Html msg
input class type_ attr inputMsg changeMsg field =
  H.input
    ( [ type_ |> A.type_
      , field |> Field.id |> A.id
      , inputMsg  |> E.onInput
      , changeMsg |> onChange
      ]
      ++ (class |> classAttr)
      ++ attr
    )
    []

textarea       = textareaInput 8  []
textareaLarge  = textareaInput 12 ["is-large"]
textareaXLarge = textareaInput 16 ["is-xlarge"]

textareaInput : Int -> List String -> List (H.Attribute msg) -> (String -> msg) -> msg -> Field.Model String -> Html msg
textareaInput rows class attr inputMsg changeMsg field =
  H.textarea
    ( [ field |> Field.id |> A.id
      , rows  |> A.rows
      , inputMsg  |> E.onInput
      , changeMsg |> onChange
      ]
      ++ (class |> classAttr)
      ++ attr
    )
    []

select : List ( String, String ) -> List (H.Attribute msg) -> (String -> msg) -> msg -> Field.Model String -> Html msg
select options attr inputMsg changeMsg field =
  H.select
    ( [ field |> Field.id |> A.id
      , inputMsg  |> E.onInput
      , changeMsg |> onChange
      ]
      ++ attr
    )
    ( options |> List.map
      (\(value,label) ->
        H.option
          [ value |> A.value
          , (value == (field |> Field.value)) |> A.selected
          ]
          [ label |> H.text ]
      )
    )

radio       = radioList []
radioInline = radioList ["is-inline"]

radioList : List String -> List ( String, String ) -> List (H.Attribute msg) -> (String -> msg) -> msg -> Field.Model String -> Html msg
radioList class options attr inputMsg changeMsg field =
  H.ul
    ( (class |> classAttr)
      ++ attr
    )
    ( options |> List.map
      (\(value,label) ->
        H.li []
          [ H.label []
            [ H.input
              [ "radio" |> A.type_
              , inputMsg  |> E.onInput
              , changeMsg |> onChange
              , value     |> A.value
              , field |> Field.value |> (==) value |> A.checked
              ] []
            , " " |> H.text
            , label |> H.text
            ]
          ]
      )
    )

checkbox       = checkList []
checkboxInline = checkList ["is-inline"]
checkboxBlock  = checkList ["is-block"]

checkList : List String -> List ( String, String ) -> List (H.Attribute msg) -> (String -> msg) -> msg -> Field.Model (Set String) -> Html msg
checkList class options attr inputMsg changeMsg field =
  H.ul
    ( (class |> classAttr)
      ++ attr
    )
    ( options |> List.map
      (\(value,label) ->
        H.li []
          [ H.label []
            [ H.input
              [ "checkbox" |> A.type_
              , inputMsg  |> E.onInput
              , changeMsg |> onChange
              , value     |> A.value
              , field |> Field.value |> Set.member value |> A.checked
              ] []
            , " " |> H.text
            , label |> H.text
            ]
          ]
      )
    )

classAttr : List String -> List (H.Attribute msg)
classAttr class =
  if class |> List.isEmpty
    then []
    else [ class |> String.join " " |> A.class ]


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

onChange : msg -> H.Attribute msg
onChange msg =
  Decode.succeed msg |> E.on "change"

paging : (Paging -> String) -> (String -> msg) -> Paging -> Html msg
paging i18n msg info =
  let
    options =
      List.range 0 (info.max - 1) |> List.map
        (\page ->
          ( page |> String.fromInt
          , page == info.page
          , { page = page, max = info.max } |> i18n
          )
        )
  in
    if options |> List.isEmpty
      then "" |> H.text
      else
        H.nav []
          [ H.select [ msg |> E.onInput ]
            ( options |> List.map
              (\(value,selected,label) ->
                H.option
                  [ value    |> A.value
                  , selected |> A.selected
                  ]
                  [ label |> H.text ]
              )
            )
          ]
