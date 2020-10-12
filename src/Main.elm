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
    FamilyTree


type FamilyTree
    = Node
        { nationality : Nationality.Distribution
        , father : FamilyTree
        , mother : FamilyTree
        }
    | Unknown Nationality.Distribution


foldTree : (Nationality.Distribution -> b -> b -> b) -> (Nationality.Distribution -> b) -> FamilyTree -> b
foldTree nodeFunc emptyFunc tree =
    case tree of
        Node { nationality, father, mother } ->
            nodeFunc nationality
                (foldTree nodeFunc emptyFunc father)
                (foldTree nodeFunc emptyFunc mother)

        Unknown nationality ->
            emptyFunc nationality


francais : Nationality.Distribution
francais =
    Dict.fromList [ ( "Français", 1 ) ]


anglais : Nationality.Distribution
anglais =
    Dict.fromList [ ( "Anglais", 1 ) ]


fiftyFifty : Nationality.Distribution
fiftyFifty =
    Dict.fromList [ ( "Français", 1 ), ( "Anglais", 1 ) ]


ancestors : FamilyTree
ancestors =
    Node
        { nationality = fiftyFifty
        , father =
            Node
                { nationality = francais
                , father =
                    Node
                        { nationality = francais
                        , father = Unknown francais
                        , mother = Unknown francais
                        }
                , mother =
                    Node
                        { nationality = francais
                        , father = Unknown francais
                        , mother = Unknown francais
                        }
                }
        , mother =
            Node
                { nationality = anglais
                , father = Unknown anglais
                , mother = Unknown anglais
                }
        }


{-| Generate a tree with 50% chance of getting a leaf node on every roll
-}
familyTreeGen : Generator FamilyTree
familyTreeGen =
    Random.weighted ( 50, nodeGen ) [ ( 50, emptyNodeGen ) ]
        |> Random.andThen identity


nodeGen : Generator FamilyTree
nodeGen =
    Random.map3 (\nat father mother -> Node { nationality = nat, father = father, mother = mother })
        Nationality.distributionGenerator
        (Random.lazy (\_ -> familyTreeGen))
        (Random.lazy (\_ -> familyTreeGen))


emptyNodeGen : Generator FamilyTree
emptyNodeGen =
    Random.map Unknown Nationality.distributionGenerator



-- UPDATE


type Msg
    = GenerateTreeClicked
    | ReceiveNewTree FamilyTree


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( ancestors, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTreeClicked ->
            ( model, Random.generate ReceiveNewTree familyTreeGen )

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


individual : Nationality.Distribution -> Html a -> Html a -> Html a
individual nationality fatherHtml motherHtml =
    Html.ul []
        [ Html.div [ Html.Attributes.class "parents" ]
            [ fatherHtml
            , motherHtml
            ]
        , Html.li []
            [ Nationality.asPieChart nationality
            ]
        ]


unknown : Nationality.Distribution -> Html a
unknown nationality =
    Html.ul []
        [ Html.li []
            [ Nationality.asPieChart nationality
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
