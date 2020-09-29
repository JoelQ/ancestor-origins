module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events
import Random exposing (Generator)


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


ancestorGen : Generator (Tree String)
ancestorGen =
    nodeOrEmpty ancestorIdGen


ancestorIdGen : Generator String
ancestorIdGen =
    Random.int 1 10000
        |> Random.map (\n -> "Ancestor id-" ++ String.fromInt n)


nodeOrEmpty : Generator a -> Generator (Tree a)
nodeOrEmpty labelGen =
    Random.weighted ( 50, nodeGen labelGen ) [ ( 50, emptyNodeGen ) ]
        |> Random.andThen identity


nodeGen : Generator a -> Generator (Tree a)
nodeGen labelGen =
    Random.map3 Node
        labelGen
        (Random.lazy (\_ -> nodeOrEmpty labelGen))
        (Random.lazy (\_ -> nodeOrEmpty labelGen))


emptyNodeGen : Generator (Tree a)
emptyNodeGen =
    Random.constant Empty



-- UPDATE


type Msg
    = GenerateTreeClicked
    | ReceiveNewTree (Tree String)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( ancestors, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTreeClicked ->
            ( model, Random.generate ReceiveNewTree ancestorGen )

        ReceiveNewTree tree ->
            ( tree, Cmd.none )



-- VIEW


view : Tree String -> Html Msg
view tree =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Ancestor - Origins" ]
        , Html.button [ Html.Events.onClick GenerateTreeClicked ]
            [ Html.text "Generate Random Tree" ]
        , foldTree individual unknown tree
        ]


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
