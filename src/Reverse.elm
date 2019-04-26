module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String }


init : Model
init =
    { content = "" }



-- updaate


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }



-- view


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "type anything here..", value model.content, onInput Change ] []
        , div [] [ text (String.reverse model.content) ]
        ]
