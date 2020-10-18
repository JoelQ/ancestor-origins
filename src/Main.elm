module Main exposing (main)

import Browser
import Color exposing (Color)
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
    { tree : FamilyTree
    , selected : Maybe Int
    }


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
    | NodeSelected Int
    | ModalCloseClicked
    | NationalityChosen Int String


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { selected = Nothing, tree = ancestors }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateTreeClicked ->
            ( model, Random.generate ReceiveNewTree FamilyTree.generator )

        ReceiveNewTree tree ->
            ( { model | tree = tree }, Cmd.none )

        NodeSelected idx ->
            ( { model | selected = Just idx }, Cmd.none )

        ModalCloseClicked ->
            ( { model | selected = Nothing }, Cmd.none )

        NationalityChosen idx newNat ->
            ( { model
                | selected = Nothing
                , tree =
                    FamilyTree.updateAt idx
                        (FamilyTree.setNationalityOfUnknownAncestors <| Dict.fromList [ ( newNat, 1 ) ])
                        model.tree
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_ []
        [ header
        , legend
        , controls
        , treeView <| FamilyTree.recalculateNationalities model.tree
        , modal model.selected model.tree
        ]


header : Html a
header =
    Html.h1 [] [ Html.text "Ancestor - Origins" ]


controls : Html Msg
controls =
    Html.button [ Html.Events.onClick GenerateTreeClicked ]
        [ Html.text "Generate Random Tree" ]


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


treeView : FamilyTree -> Html Msg
treeView =
    FamilyTree.foldWithIndex individual unknown


individual : Int -> Nationality.Distribution -> Html Msg -> Html Msg -> Html Msg
individual id nationality fatherHtml motherHtml =
    Html.ul []
        [ Html.div [ Html.Attributes.class "parents" ]
            [ fatherHtml
            , motherHtml
            ]
        , Html.li [ Html.Events.onClick (NodeSelected id) ]
            [ Nationality.asPieChart nationality
            , Html.text <| String.fromInt id
            ]
        ]


unknown : Int -> Nationality.Distribution -> Html Msg
unknown id nationality =
    Html.ul []
        [ Html.li [ Html.Events.onClick (NodeSelected id) ]
            [ Nationality.asMutedPieChart nationality
            , Html.text <| String.fromInt id
            ]
        ]


modal : Maybe Int -> FamilyTree -> Html Msg
modal maybeIdx tree =
    case maybeIdx of
        Nothing ->
            Html.text ""

        Just idx ->
            case FamilyTree.find idx tree of
                Nothing ->
                    Html.text ""

                Just nat ->
                    Html.section [ Html.Attributes.class "modal" ]
                        [ Html.h1 [] [ Html.text "Set Nationality" ]
                        , Html.button [ Html.Attributes.class "close", Html.Events.onClick ModalCloseClicked ] [ Html.text "x" ]
                        , Html.div [ Html.Attributes.class "modal-content" ]
                            [ nationalityForm idx ]
                        ]


nationalityForm : Int -> Html Msg
nationalityForm idx =
    let
        colors =
            Nationality.colorMap |> Dict.toList
    in
    Html.div [ Html.Attributes.class "nationality-form" ] <|
        List.map
            (\( nationality, color ) ->
                chooseNationalityButton (NationalityChosen idx) nationality color
            )
            colors


chooseNationalityButton : (String -> Msg) -> String -> Color -> Html Msg
chooseNationalityButton natTagger nationality color =
    Html.button
        [ Html.Attributes.style "color" <| Color.toCssString color
        , Html.Events.onClick (natTagger nationality)
        ]
        [ Html.text nationality ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
