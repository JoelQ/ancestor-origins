module Main exposing (main)

import Browser
import Dict exposing (Dict)
import FamilyTree exposing (FamilyTree(..))
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Nationality
import Random


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
            ( model, Random.generate ReceiveNewTree FamilyTree.generator )

        ReceiveNewTree tree ->
            ( tree, Cmd.none )



-- VIEW


view : Model -> Html Msg
view tree =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Ancestor - Origins" ]
        , Html.button [ Html.Events.onClick GenerateTreeClicked ]
            [ Html.text "Generate Random Tree" ]
        , FamilyTree.fold individual unknown tree
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
            [ Nationality.asMutedPieChart nationality
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
