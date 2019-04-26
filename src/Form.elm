module Main exposing (Model, main)

import Browser



-- MODEL


main =
    Browser.sandbox { model = model }


type alias Model =
    { email : String, password : String }


init : Model
init =
    { email = "", password = "" }
