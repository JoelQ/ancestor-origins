module Nationality exposing
    ( Distribution
    , asMutedPieChart
    , asPieChart
    , distributionGenerator
    , merge
    )

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
    Dict String Float


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


merge : Distribution -> Distribution -> Distribution
merge d1 d2 =
    let
        ( scaled1, scaled2 ) =
            scaleToLargest d1 d2
    in
    Dict.merge
        (\k v dict -> Dict.insert k (Debug.log "left" v) dict)
        (\key val1 val2 dict -> Dict.insert key (Debug.log "both" <| val1 + val2) dict)
        (\k v dict -> Dict.insert k (Debug.log "right" v) dict)
        scaled1
        scaled2
        Dict.empty


scaleToLargest : Distribution -> Distribution -> ( Distribution, Distribution )
scaleToLargest d1 d2 =
    let
        ( min, max ) =
            minMax d1 d2

        factor =
            totalNatValue max / totalNatValue min

        scaled =
            Dict.map (\_ val -> val * factor) min
    in
    ( scaled, max )


totalNatValue : Distribution -> Float
totalNatValue d =
    Dict.foldr (\_ v memo -> v + memo) 0 d


minMax : Distribution -> Distribution -> ( Distribution, Distribution )
minMax d1 d2 =
    if totalNatValue d1 > totalNatValue d2 then
        ( d2, d1 )

    else
        ( d1, d2 )



-- SVG


asPieChart : Distribution -> Html a
asPieChart =
    pieChartHelp nationalSlice


asMutedPieChart : Distribution -> Html a
asMutedPieChart =
    pieChartHelp nationalSliceMuted


pieChartHelp : (String -> Shape.Arc -> Svg a) -> Distribution -> Html a
pieChartHelp viewSlice stats =
    let
        arcs =
            Shape.pie Shape.defaultPieConfig (Dict.values stats)

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
            List.map2 viewSlice
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
        , Svg.Attributes.stroke <| Color.toCssString color
        ]


nationalSliceMuted : String -> Shape.Arc -> Svg a
nationalSliceMuted label arc =
    let
        color =
            Dict.get label colorMap
                |> Maybe.withDefault Color.black
                |> muted
    in
    Path.element (Shape.arc arc)
        [ Svg.Attributes.fill <| Color.toCssString color
        , Svg.Attributes.stroke <| Color.toCssString color
        , Svg.Attributes.strokeDasharray "10,10"
        , Svg.Attributes.strokeWidth "5"
        ]


muted : Color -> Color
muted color =
    let
        components =
            Color.toRgba color
    in
    Color.fromRgba { components | alpha = 0.5 }



-- RANDOM GENERATORS


distributionGenerator : Generator Distribution
distributionGenerator =
    all
        |> takeRandom
        |> Random.andThen (combineMap natPair)
        |> Random.map Dict.fromList


natPair : String -> Generator ( String, Float )
natPair name =
    Random.pair (Random.constant name) (Random.float 1 10)



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
