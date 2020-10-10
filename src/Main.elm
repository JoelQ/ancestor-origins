module Main exposing (main)

import Browser
import Dict exposing (Dict)
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
    Tree Person


type alias Person =
    { name : String
    , nationality : Dict String Int
    }


nationalities : List String
nationalities =
    [ "Français", "Anglais", "Écossais", "Allemand" ]


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
    Node (Person "Self" Dict.empty)
        (Node (Person "Father" Dict.empty) Empty Empty)
        (Node (Person "Mother" Dict.empty) Empty Empty)


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
    takeRandom nationalities
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
        [ Html.li [] [ Html.text person.name ]
        , nationality person.nationality
        , fatherHtml
        , motherHtml
        ]


asPercentageOf : Int -> Int -> Int
asPercentageOf total val =
    round ((toFloat val / toFloat total) * 100)


percent : Int -> String
percent n =
    String.fromInt n ++ "%"


nationality : Dict String Int -> Html a
nationality stats =
    let
        total =
            stats |> Dict.values |> List.sum
    in
    Html.text <|
        "("
            ++ (stats
                    |> Dict.toList
                    |> List.map
                        (\( name, value ) ->
                            name ++ " - " ++ (percent <| asPercentageOf total <| value)
                        )
                    |> String.join " | "
               )
            ++ ")"


unknown : Html a
unknown =
    Html.ul []
        [ Html.li [] [ Html.text "Unknown" ]
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
