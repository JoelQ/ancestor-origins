module Main exposing (main)

import Browser
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Path
import Random exposing (Generator)
import Scale.Color
import Shape
import Svg exposing (Svg)
import Svg.Attributes


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
    , nationality : Dict String Int
    }


nationalities : Dict String Color
nationalities =
    Dict.fromList <|
        List.map2 Tuple.pair
            [ "Français", "Anglais", "Écossais", "Allemand" ]
            Scale.Color.category10


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
        nationalityGen


nameGen : Generator String
nameGen =
    Random.int 1 10000
        |> Random.map (\id -> "Ancestor id-" ++ String.fromInt id)


nationalityGen : Generator (Dict String Int)
nationalityGen =
    Dict.keys nationalities
        |> takeRandom
        |> Random.andThen (combineMap natPair)
        |> Random.map Dict.fromList


natPair : String -> Generator ( String, Int )
natPair name =
    Random.pair (Random.constant name) (Random.int 1 10)


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
            [ nationality person.nationality
            ]
        ]


nationality : Dict String Int -> Html a
nationality stats =
    let
        floats =
            List.map toFloat (Dict.values stats)

        arcs =
            Shape.pie Shape.defaultPieConfig floats

        radius =
            Shape.defaultPieConfig.outerRadius

        width =
            radius * 2

        height =
            radius * 2

        translation =
            "translate("
                ++ String.fromFloat radius
                ++ ","
                ++ String.fromFloat radius
                ++ ")"

        viewBoxString =
            "0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height
    in
    Svg.svg [ Svg.Attributes.viewBox viewBoxString ]
        [ Svg.g [ Svg.Attributes.transform translation ] <|
            List.map2 nationalSlice
                (Dict.keys stats)
                arcs
        ]


nationalSlice : String -> Shape.Arc -> Svg a
nationalSlice label arc =
    let
        color =
            Dict.get label nationalities
                |> Maybe.withDefault Color.black
    in
    Path.element (Shape.arc arc)
        [ Svg.Attributes.fill <| Color.toCssString color
        ]


unknown : Html a
unknown =
    Html.ul []
        [ Html.li []
            [ nationality <| Dict.fromList [ ( "unkown", 1 ) ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- RANDOM HELPERS


takeRandom : List a -> Generator (List a)
takeRandom list =
    Random.int 1 (List.length list)
        |> Random.map (\n -> List.take n list)


combineMap : (a -> Generator b) -> List a -> Generator (List b)
combineMap func =
    List.foldr
        (\item acc -> Random.map2 (::) (func item) acc)
        (Random.constant [])
