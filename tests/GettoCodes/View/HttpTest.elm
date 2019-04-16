module GettoCodes.View.HttpTest exposing (..)
import GettoCodes.View.Http as HttpView

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Http"
    [ describe "empty"
      [ test "should return empty model" <|
        \_ ->
          let
            model = HttpView.empty
          in
            ( model |> HttpView.state
            , model |> HttpView.response
            )
            |> Expect.equal
              ( HttpView.Ready Nothing
              , Nothing
              )
      ]

    , describe "update"
      [ test "should update model to load state" <|
        \_ ->
          let
            model =
              HttpView.empty
              |> HttpView.update HttpView.load
          in
            ( model |> HttpView.state
            , model |> HttpView.response
            )
            |> Expect.equal
              ( HttpView.Connecting Nothing
              , Nothing
              )

      , test "should update model to transfer state" <|
        \_ ->
          let
            progress = HttpView.Sending { current = 0, size = 100 }
            model =
              HttpView.empty
              |> HttpView.update (HttpView.transfer progress)
          in
            ( model |> HttpView.state
            , model |> HttpView.response
            )
            |> Expect.equal
              ( HttpView.Connecting (Just progress)
              , Nothing
              )

      , test "should update model to not modified state" <|
        \_ ->
          let
            model =
              HttpView.empty
              |> HttpView.update HttpView.notModified
          in
            ( model |> HttpView.state
            , model |> HttpView.response
            )
            |> Expect.equal
              ( HttpView.Ready Nothing
              , Nothing
              )

      , test "should update model to failure state" <|
        \_ ->
          let
            model =
              HttpView.empty
              |> HttpView.update (HttpView.failure HttpView.BadRequest)
          in
            ( model |> HttpView.state
            , model |> HttpView.response
            )
            |> Expect.equal
              ( HttpView.Ready (Just HttpView.BadRequest)
              , Nothing
              )

      , test "should update model to success state" <|
        \_ ->
          let
            response = { body = "body" } |> HttpView.toResponse { header = "header" }
            model =
              HttpView.empty
              |> HttpView.update (HttpView.success response)
          in
            ( model |> HttpView.state
            , model |> HttpView.response
            )
            |> Expect.equal
              ( HttpView.Ready Nothing
              , Just response
              )

      , test "should update model to success state from failure state" <|
        \_ ->
          let
            response = { body = "body" } |> HttpView.toResponse { header = "header" }
            model =
              HttpView.empty
              |> HttpView.update (HttpView.failure HttpView.BadRequest)
              |> HttpView.update (HttpView.success response)
          in
            ( model |> HttpView.state
            , model |> HttpView.response
            )
            |> Expect.equal
              ( HttpView.Ready Nothing
              , Just response
              )

      , test "should clear model response" <|
        \_ ->
          let
            response = { body = "body" } |> HttpView.toResponse { header = "header" }
            model =
              HttpView.empty
              |> HttpView.update (HttpView.success response)
              |> HttpView.clear
          in
            ( model |> HttpView.state
            , model |> HttpView.response
            )
            |> Expect.equal
              ( HttpView.Ready Nothing
              , Nothing
              )

      , test "should clear model response from failure state" <|
        \_ ->
          let
            model =
              HttpView.empty
              |> HttpView.update (HttpView.failure HttpView.BadRequest)
              |> HttpView.clear
          in
            ( model |> HttpView.state
            , model |> HttpView.response
            )
            |> Expect.equal
              ( HttpView.Ready (Just HttpView.BadRequest)
              , Nothing
              )
      ]

    , describe "isSuccess"
      [ test "should return response when success migration" <|
        \_ ->
          let
            response = { body = "body" } |> HttpView.toResponse { header = "header" }
            mig = HttpView.success response
          in
            mig
            |> HttpView.isSuccess
            |> Expect.equal
              (Just response)

      , test "should return nothing when other migration" <|
        \_ ->
          [ HttpView.load |> HttpView.isSuccess
          , HttpView.transfer HttpView.Proccessing |> HttpView.isSuccess
          , HttpView.notModified |> HttpView.isSuccess
          , HttpView.failure HttpView.BadRequest |> HttpView.isSuccess
          ]
          |> Expect.equal
            [ Nothing
            , Nothing
            , Nothing
            , Nothing
            ]
      ]

    , describe "isComplete"
      [ test "should return true when success, failure migration" <|
        \_ ->
          let
            response = { body = "body" } |> HttpView.toResponse { header = "header" }
          in
            [ HttpView.success response |> HttpView.isComplete
            , HttpView.failure HttpView.BadRequest |> HttpView.isComplete
            ]
            |> Expect.equal
              [ True
              , True
              ]

      , test "should return false when other migration" <|
        \_ ->
          [ HttpView.load |> HttpView.isComplete
          , HttpView.transfer HttpView.Proccessing |> HttpView.isComplete
          , HttpView.notModified |> HttpView.isComplete
          ]
          |> Expect.equal
            [ False
            , False
            , False
            ]
      ]

    , describe "header"
      [ test "should return header from response" <|
        \_ ->
          let
            header = { header = "header" }
            body = { body = "body" }
            response = body |> HttpView.toResponse header
          in
            response
            |> HttpView.header
            |> Expect.equal
              header
      ]

    , describe "body"
      [ test "should return body from response" <|
        \_ ->
          let
            header = { header = "header" }
            body = { body = "body" }
            response = body |> HttpView.toResponse header
          in
            response
            |> HttpView.body
            |> Expect.equal
              body
      ]

    , describe "progress"
      [ test "should return progress info from sending data" <|
        \_ ->
          let
            progress = HttpView.Sending { current = 10, size = 100 }
          in
            (Just progress)
            |> HttpView.progress
            |> Expect.equal
              ( Just
                ( True
                , 10
                )
              )

      , test "should return progress info from receiving data" <|
        \_ ->
          let
            progress = HttpView.Receiving (Just { current = 10, size = 100 })
          in
            (Just progress)
            |> HttpView.progress
            |> Expect.equal
              ( Just
                ( False
                , 90
                )
              )

      , test "should return nothing from receiving data that has no progress info" <|
        \_ ->
          let
            progress = HttpView.Receiving Nothing
          in
            (Just progress)
            |> HttpView.progress
            |> Expect.equal
              Nothing

      , test "should return nothing from other progress data" <|
        \_ ->
          let
            progress = HttpView.Proccessing
          in
            (Just progress)
            |> HttpView.progress
            |> Expect.equal
              Nothing

      , test "should return nothing from no progress data" <|
        \_ ->
          Nothing
          |> HttpView.progress
          |> Expect.equal
            Nothing
      ]

    , describe "isConflict"
      [ test "should return true when model has conflict error" <|
        \_ ->
          let
            model =
              HttpView.empty
              |> HttpView.update (HttpView.failure HttpView.Conflict)
          in
            model
            |> HttpView.isConflict
            |> Expect.equal
              True

      , test "should return false when model has not conflict error" <|
        \_ ->
          HttpView.empty
          |> HttpView.isConflict
          |> Expect.equal
            False
      ]
    ]
