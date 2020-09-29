module Main exposing (main)

import Browser
import Html exposing (Html)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Flags =
    ()


type alias Model =
    Tree String


type Tree a
    = Node a (Tree a) (Tree a)
    | Empty


foldTree : (a -> b -> b -> b) -> b -> Tree a -> b
foldTree func initial tree =
    case tree of
        Node val left right ->
            func val (foldTree func initial left) (foldTree func initial right)

        Empty ->
            initial


ancestors : Tree String
ancestors =
    Node "Self"
        (Node "Father" Empty Empty)
        (Node "Mother" Empty Empty)



-- UPDATE


type Msg
    = Noop


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( ancestors, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )



-- VIEW


view : Tree String -> Html a
view tree =
    foldTree individual unknown tree


individual : String -> Html a -> Html a -> Html a
individual name fatherHtml motherHtml =
    Html.ul []
        [ Html.li [] [ Html.text name ]
        , fatherHtml
        , motherHtml
        ]


unknown : Html a
unknown =
    Html.ul []
        [ Html.li [] [ Html.text "Unknown" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
