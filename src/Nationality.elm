module Nationality exposing (Distribution, asPieChart, distributionGenerator)

import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
import Path
import Random exposing (Generator)
import Scale.Color
import Shape
import Svg exposing (Svg)
import Svg.Attributes


type alias Distribution =
    Dict String Int


type alias ColorMap =
    Dict String Color


all : List String
all =
    [ "Français", "Anglais", "Écossais", "Allemand" ]


colorMap : Dict String Color
colorMap =
    Dict.fromList <|
        List.map2 Tuple.pair
            all
            Scale.Color.category10



-- SVG


asPieChart : Distribution -> Html a
asPieChart stats =
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
            Dict.get label colorMap
                |> Maybe.withDefault Color.black
    in
    Path.element (Shape.arc arc)
        [ Svg.Attributes.fill <| Color.toCssString color
        ]



-- RANDOM GENERATORS


distributionGenerator : Generator (Dict String Int)
distributionGenerator =
    all
        |> takeRandom
        |> Random.andThen (combineMap natPair)
        |> Random.map Dict.fromList


natPair : String -> Generator ( String, Int )
natPair name =
    Random.pair (Random.constant name) (Random.int 1 10)



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
