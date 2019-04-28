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
    { email : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" ""



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
        div [] []

    else
        let
            validations =
                applyValidators2 basicPasswordValidators model.password model.passwordAgain
        in
        if List.isEmpty validations then
            div [ style "color" "green" ] [ text "OK" ]

        else
            div [] <| List.map validationFeedback validations


validationFeedback : Validation -> Html msg
validationFeedback t =
    div [ style "color" "red" ] [ text (Tuple.second t) ]
