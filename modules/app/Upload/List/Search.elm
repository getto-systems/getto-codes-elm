module GettoUpload.App.Upload.List.Search exposing
  ( Model
  , Msg
  , init
  , query
  , queryChanged
  , store
  , storeChanged
  , subscriptions
  , update
  , contents
  , dialogs
  )
import GettoUpload.App.Upload.List.Search.Html as Html
import GettoUpload.Layout.Page.Page as Layout
import GettoUpload.Layout.Frame as Frame
import GettoUpload.I18n.App as I18n
import GettoUpload.I18n.App.Upload.List.Search as SearchI18n

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

import File exposing ( File )
import File.Select
import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias FrameModel a = Frame.Model Layout.Model { a | search : Model }
type alias FrameTransition a = Transition (FrameModel a) Msg
type alias Model =
  { entries :
    { file :
      { name : String
      , file : Maybe File
      }
    }
  }

type Msg
  = FileRequest
  | FileSelect File
  | Upload

init : Frame.InitModel -> ( Model, FrameTransition a )
init model =
  ( { entries =
      { file =
        { name = "vendor-order"
        , file = Nothing
        }
      }
    }
  , Transition.none
  )

query : Model -> QueryEncode.Value
query model = QueryEncode.empty

queryChanged : List String -> QueryDecode.Value -> Model -> Model
queryChanged names value model = model

store : Model -> Encode.Value
store model = Encode.null

storeChanged : Decode.Value -> Model -> Model
storeChanged value model = model

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> ( Model, FrameTransition a )
update msg model =
  case msg of
    FileRequest -> ( model, \_ -> FileSelect |> File.Select.file [] )
    FileSelect selected ->
      let
        entries = model.entries
        file = entries.file
      in
        ( { model | entries = { entries | file = { file | file = Just selected } } }
        , Transition.none
        )

    Upload -> ( model, Transition.none )

contents : FrameModel a -> List (Html Msg)
contents model =
  [ model |> list
  ]

list : FrameModel a -> Html Msg
list model =
  H.section [ A.class "edit" ]
    [ model |> search
    ]

search : FrameModel a -> Html Msg
search model = L.lazy
  (\s -> Html.search
    { title = "select-file"
    , entries = s.entries
    , msg =
      { submit = Upload
      , select = FileRequest
      }
    , i18n =
      { title = SearchI18n.title
      , entry = SearchI18n.entry
      , form  = I18n.form
      }
    }
  )
  (model |> Frame.app |> .search)

dialogs : FrameModel a -> List (Html Msg)
dialogs model = []
