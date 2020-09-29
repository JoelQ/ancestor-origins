module Main exposing (main)

import Html exposing (Html)


main : Html a
main =
    view ancestors


type Tree a
    = Node a (Tree a) (Tree a)
    | Empty


ancestors : Tree String
ancestors =
    Node "Self"
        (Node "Father" Empty Empty)
        (Node "Mother" Empty Empty)


view : Tree String -> Html a
view tree =
    case tree of
        Node name father mother ->
            Html.ul []
                [ Html.li [] [ Html.text name ]
                , view father
                , view mother
                ]

        Empty ->
            Html.ul []
                [ Html.li [] [ Html.text "Unknown" ]
                ]
