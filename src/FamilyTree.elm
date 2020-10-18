module FamilyTree exposing
    ( FamilyTree(..)
    , find
    , fold
    , foldWithIndex
    , generator
    , recalculateNationalities
    , setNationalityOfUnknownAncestors
    , updateAt
    )

import Dict
import Maybe.Extra
import Nationality
import Random exposing (Generator)


type FamilyTree
    = Node
        { nationality : Nationality.Distribution
        , father : FamilyTree
        , mother : FamilyTree
        }
    | Unknown Nationality.Distribution


find : Int -> FamilyTree -> Maybe Nationality.Distribution
find needle =
    foldWithIndex
        (\idx nat fatherSearch motherSearch ->
            if idx == needle then
                Just nat

            else
                Maybe.Extra.or fatherSearch motherSearch
        )
        (\idx nat ->
            if idx == needle then
                Just nat

            else
                Nothing
        )


fold :
    (Nationality.Distribution -> b -> b -> b)
    -> (Nationality.Distribution -> b)
    -> FamilyTree
    -> b
fold nodeFunc emptyFunc =
    foldWithIndex (always nodeFunc) (always emptyFunc)


foldWithIndex :
    (Int -> Nationality.Distribution -> b -> b -> b)
    -> (Int -> Nationality.Distribution -> b)
    -> FamilyTree
    -> b
foldWithIndex nodeFunc emptyFunc =
    Tuple.first << foldHelp 0 nodeFunc emptyFunc


foldHelp :
    Int
    -> (Int -> Nationality.Distribution -> b -> b -> b)
    -> (Int -> Nationality.Distribution -> b)
    -> FamilyTree
    -> ( b, Int )
foldHelp index nodeFunc emptyFunc tree =
    case tree of
        Node { nationality, father, mother } ->
            let
                ( fatherAcc, afterFatherIndex ) =
                    foldHelp (index + 1) nodeFunc emptyFunc father

                ( motherAcc, afterMotherIndex ) =
                    foldHelp afterFatherIndex nodeFunc emptyFunc mother
            in
            ( nodeFunc index nationality fatherAcc motherAcc, afterMotherIndex )

        Unknown nationality ->
            ( emptyFunc index nationality, index + 1 )


getNationality : FamilyTree -> Nationality.Distribution
getNationality tree =
    case tree of
        Node { nationality } ->
            nationality

        Unknown nationality ->
            nationality


setNationalityOfUnknownAncestors : Nationality.Distribution -> FamilyTree -> FamilyTree
setNationalityOfUnknownAncestors newNat =
    fold
        (\nat father mother -> Node { nationality = nat, father = father, mother = mother })
        (\_ -> Unknown newNat)


updateAt : Int -> (FamilyTree -> FamilyTree) -> FamilyTree -> FamilyTree
updateAt id func tree =
    foldWithIndex
        (\idx nat father mother ->
            if idx == id then
                func (Node { nationality = nat, father = father, mother = mother })

            else
                Node { nationality = nat, father = father, mother = mother }
        )
        (\idx nat ->
            if idx == id then
                func (Unknown nat)

            else
                Unknown nat
        )
        tree


recalculateNationalities : FamilyTree -> FamilyTree
recalculateNationalities tree =
    fold
        (\nat father mother ->
            Node
                { nationality = Nationality.merge (getNationality father) (getNationality mother)
                , father = father
                , mother = mother
                }
        )
        (\nat -> Unknown nat)
        tree



-- RANDOM GENERATORS


maxDepth : Int
maxDepth =
    5


{-| Generate a tree with 50% chance of getting a leaf node on every roll
-}
generator : Generator FamilyTree
generator =
    generateFromDepth 0


generateFromDepth : Int -> Generator FamilyTree
generateFromDepth depth =
    if depth > maxDepth then
        unknownGen

    else
        Random.weighted ( 50, nodeGen depth ) [ ( 50, unknownGen ) ]
            |> Random.andThen identity


nodeGen : Int -> Generator FamilyTree
nodeGen depth =
    Random.map3 (\nat father mother -> Node { nationality = nat, father = father, mother = mother })
        (Random.constant Dict.empty)
        (Random.lazy (\_ -> generateFromDepth <| depth + 1))
        (Random.lazy (\_ -> generateFromDepth <| depth + 1))


unknownGen : Generator FamilyTree
unknownGen =
    Random.map Unknown Nationality.singleNationalityGenerator
