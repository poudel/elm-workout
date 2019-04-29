module Farenheit exposing (main)

import Browser
import Html exposing (Attribute, Html, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MODEL


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { input : String }


init : Model
init =
    { input = "" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change i ->
            { model | input = i }



-- VIEW
-- view : Model -> Html Msg
-- view model =
--     case String.toFloat model.input of
--         Just
