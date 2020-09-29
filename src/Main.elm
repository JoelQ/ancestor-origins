module Main exposing (main)

import Html exposing (Html)


main : Html a
main =
    view ancestors


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


ancestors : Tree String
ancestors =
    Node "Self"
        (Node "Father" Empty Empty)
        (Node "Mother" Empty Empty)


view : Tree String -> Html a
view tree =
    foldTree individual unknown tree


individual : String -> Html a -> Html a -> Html a
individual name fatherHtml motherHtml =
    Html.ul []
        [ Html.li [] [ Html.text name ]
        , fatherHtml
        , motherHtml
        ]


unknown : Html a
unknown =
    Html.ul []
        [ Html.li [] [ Html.text "Unknown" ]
        ]
