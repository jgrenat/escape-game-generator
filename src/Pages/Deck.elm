module Pages.Deck exposing (view)

import Array exposing (Array)
import Views.Cards.StaticCard as CardView
import CardEditor.Card as Card
import Html exposing (Html, div, h2, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (flex, flex_wrap, mb2, mr2, pv2)


view : msg -> (Card.Model -> msg) -> Array Card.Model -> Html msg
view onCreate onCardClicked cards =
    div []
        [ h2 [] [ text "My deck" ]
        , viewControls onCreate
        , viewCards onCardClicked cards
        ]


viewCards : (Card.Model -> msg) -> Array Card.Model -> Html msg
viewCards onCardClicked cards =
    cards
        |> Array.map (\card -> ( card, CardView.view "300px" "600px" card ))
        |> Array.toList
        |> List.map
            (\( card, cardHtml ) ->
                div
                    [ classes [ mr2, mb2 ]
                    , style [ ( "zoom", "0.5" ) ]
                    , onClick (onCardClicked card)
                    ]
                    [ cardHtml ]
            )
        |> div [ classes [ flex, flex_wrap ] ]


viewControls : msg -> Html.Html msg
viewControls createMsg =
    div [ classes [ pv2 ] ] [ button [ onClick createMsg ] [ text "Create a new card" ] ]
