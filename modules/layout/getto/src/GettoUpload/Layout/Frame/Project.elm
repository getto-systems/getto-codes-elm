module GettoUpload.Layout.Frame.Project exposing
  ( Model
  , decode
  , name
  , company
  , title
  , sub
  )

import Getto.Json.SafeDecode as SafeDecode

import Json.Decode as Decode

type Model = Model Inner
type alias Inner =
  { name    : String
  , company : String
  , title   : String
  , sub     : String
  }

decode : Decode.Value -> Model
decode value = Model
  { name    = value |> SafeDecode.at ["name"]    (SafeDecode.string "")
  , company = value |> SafeDecode.at ["company"] (SafeDecode.string "")
  , title   = value |> SafeDecode.at ["title"]   (SafeDecode.string "")
  , sub     = value |> SafeDecode.at ["sub"]     (SafeDecode.string "")
  }

name : Model -> String
name (Model model) = model.name

company : Model -> String
company (Model model) = model.company

title : Model -> String
title (Model model) = model.title

sub : Model -> String
sub (Model model) = model.sub
