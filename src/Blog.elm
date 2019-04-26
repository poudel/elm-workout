module Main exposing (BlogPost, Model, main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias BlogPost =
    { title : String, content : String }


type alias Model =
    { posts : List BlogPost }


init : Model
init =
    { posts =
        [ { title = "Post one", content = "Post one content" }
        , { title = "Post zero", content = "Post zero content" }
        ]
    }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- VIEW


viewBlogPost : BlogPost -> Html Msg
viewBlogPost post =
    div [ class "blog-item" ]
        [ div [] [ text post.title ]
        , div [] [ text post.content ]
        ]



-- what is this doing?
-- why is it piping `List.map viewBlogPost model.posts` in second list


viewBlogPostList : List BlogPost -> Html Msg
viewBlogPostList postList =
    div [] <|
        List.map viewBlogPost postList


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "blog" ]
        , lazy viewBlogPostList model.posts
        ]
