module FamilyTree exposing (FamilyTree(..), fold, generator)

import Nationality
import Random exposing (Generator)


type FamilyTree
    = Node
        { nationality : Nationality.Distribution
        , father : FamilyTree
        , mother : FamilyTree
        }
    | Unknown Nationality.Distribution


fold : (Nationality.Distribution -> b -> b -> b) -> (Nationality.Distribution -> b) -> FamilyTree -> b
fold nodeFunc emptyFunc tree =
    case tree of
        Node { nationality, father, mother } ->
            nodeFunc nationality
                (fold nodeFunc emptyFunc father)
                (fold nodeFunc emptyFunc mother)

        Unknown nationality ->
            emptyFunc nationality



-- RANDOM GENERATORS


{-| Generate a tree with 50% chance of getting a leaf node on every roll
-}
generator : Generator FamilyTree
generator =
    Random.weighted ( 50, nodeGen ) [ ( 50, unknownGen ) ]
        |> Random.andThen identity


nodeGen : Generator FamilyTree
nodeGen =
    Random.map3 (\nat father mother -> Node { nationality = nat, father = father, mother = mother })
        Nationality.distributionGenerator
        (Random.lazy (\_ -> generator))
        (Random.lazy (\_ -> generator))


unknownGen : Generator FamilyTree
unknownGen =
    Random.map Unknown Nationality.distributionGenerator
