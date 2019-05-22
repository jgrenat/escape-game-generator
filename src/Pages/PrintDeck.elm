module Pages.PrintDeck exposing (view)

import Array exposing (Array)
import Data.Card as Card
import Html exposing (..)
import Html.Attributes exposing (style)
import Views.Cards.StaticCard as StaticCard


view : Array Card.Model -> Html msg
view cards =
    Array.map (StaticCard.view "230px" "460px") cards
        |> Array.map (List.singleton >> div [ style "padding" "10px", style "page-break-inside" "avoid" ])
        |> Array.toList
        |> div
            [ style "display" "flex"
            , style "flex-wrap" "wrap"
            , style "max-width" "21cm"
            ]
