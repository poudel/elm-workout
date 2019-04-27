module Main exposing (Model, main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Password exposing (..)



-- MODEL


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { email : String, password : String, passwordAgain : String }


init : Model
init =
    { email = "", password = "", passwordAgain = "" }



-- UPDATE


type Msg
    = Email String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Email email ->
            { model | email = email }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "email" "Email" model.email Email
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Password again" model.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if List.any String.isEmpty [ model.password, model.passwordAgain ] then
        -- return empty string
        inputFeedback "" ""

    else if model.password == model.passwordAgain then
        if String.length model.password < 2 then
            inputFeedback "red" "Password must be more than 8 characters"

        else
            inputFeedback "green" "OK"

    else
        inputFeedback "red" "Password don't match"


inputFeedback : String -> String -> Html msg
inputFeedback c m =
    div [ style "color" c ] [ text m ]
