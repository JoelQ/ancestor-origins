module Main exposing (main)

import Browser
import Color
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


allemand : Nationality.Distribution
allemand =
    Dict.fromList [ ( "Allemand", 1 ) ]


irlandais : Nationality.Distribution
irlandais =
    Dict.fromList [ ( "Irlandais", 1 ) ]


ancestors : FamilyTree
ancestors =
    FamilyTree.recalculateNationalities <|
        Node
            { nationality = Dict.empty
            , father =
                Node
                    { nationality = Dict.empty
                    , father =
                        Node
                            { nationality = Dict.empty
                            , father = Unknown francais
                            , mother = Unknown allemand
                            }
                    , mother =
                        Node
                            { nationality = Dict.empty
                            , father = Unknown irlandais
                            , mother = Unknown francais
                            }
                    }
            , mother =
                Node
                    { nationality = Dict.empty
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
        , legend
        , Html.button [ Html.Events.onClick GenerateTreeClicked ]
            [ Html.text "Generate Random Tree" ]
        , FamilyTree.fold individual unknown <| FamilyTree.recalculateNationalities tree
        ]


legend : Html a
legend =
    let
        colors =
            Nationality.colorMap |> Dict.toList
    in
    Html.div [ Html.Attributes.class "legend" ] <|
        List.map
            (\( nationality, color ) ->
                Html.span
                    [ Html.Attributes.style "color" <| Color.toCssString color ]
                    [ Html.text nationality ]
            )
            colors


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
