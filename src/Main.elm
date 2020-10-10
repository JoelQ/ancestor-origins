module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Nationality
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
    Tree Person


type alias Person =
    { name : String
    , nationality : Nationality.Distribution
    }


unknownNationality : Nationality.Distribution
unknownNationality =
    Dict.fromList [ ( "Unknown", 1 ) ]


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


ancestors : Tree Person
ancestors =
    Node (Person "Self" (Dict.fromList [ ( "Français", 1 ), ( "Anglais", 1 ) ]))
        (Node (Person "Father" (Dict.fromList [ ( "Français", 1 ) ]))
            (Node (Person "Grandfather" (Dict.fromList [ ( "Français", 1 ) ])) Empty Empty)
            (Node (Person "Grandmother" (Dict.fromList [ ( "Français", 1 ) ]))
                (Node (Person "Great-Grandfather" (Dict.fromList [ ( "Français", 1 ) ])) Empty Empty)
                (Node (Person "Great-Grandmother" (Dict.fromList [ ( "Français", 1 ) ])) Empty Empty)
            )
        )
        (Node (Person "Mother" (Dict.fromList [ ( "Anglais", 1 ) ])) Empty Empty)


ancestorGen : Generator (Tree Person)
ancestorGen =
    treeGen personGen


personGen : Generator Person
personGen =
    Random.map2 Person
        nameGen
        Nationality.distributionGenerator


nameGen : Generator String
nameGen =
    Random.int 1 10000
        |> Random.map (\id -> "Ancestor id-" ++ String.fromInt id)


{-| Generate a tree with 50% chance of getting a leaf node on every roll
-}
treeGen : Generator a -> Generator (Tree a)
treeGen labelGen =
    Random.weighted ( 50, nodeGen labelGen ) [ ( 50, emptyNodeGen ) ]
        |> Random.andThen identity


nodeGen : Generator a -> Generator (Tree a)
nodeGen labelGen =
    Random.map3 Node
        labelGen
        (Random.lazy (\_ -> treeGen labelGen))
        (Random.lazy (\_ -> treeGen labelGen))


emptyNodeGen : Generator (Tree a)
emptyNodeGen =
    Random.constant Empty



-- UPDATE


type Msg
    = GenerateTreeClicked
    | ReceiveNewTree (Tree Person)


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


view : Model -> Html Msg
view tree =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Ancestor - Origins" ]
        , Html.button [ Html.Events.onClick GenerateTreeClicked ]
            [ Html.text "Generate Random Tree" ]
        , foldTree individual unknown tree
        ]


individual : Person -> Html a -> Html a -> Html a
individual person fatherHtml motherHtml =
    Html.ul []
        [ Html.div [ Html.Attributes.class "parents" ]
            [ fatherHtml
            , motherHtml
            ]
        , Html.li []
            [ Nationality.asPieChart person.nationality
            ]
        ]


unknown : Html a
unknown =
    Html.ul []
        [ Html.li []
            [ Nationality.asPieChart unknownNationality
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
