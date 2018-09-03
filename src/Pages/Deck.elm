module Pages.Deck exposing (Config, view)

import Array exposing (Array)
import Data.Card as Card
import Html exposing (Html, button, div, h2, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Tachyons.Classes exposing (absolute, bg_white_40, bg_white_60, bg_white_70, bg_white_80, br1, f3, flex, flex_wrap, mb2, mr2, pa1, pa2, pointer, pr1, pt1, pv2, relative, right_0, right_1, top_0, top_1)
import Tachyons.Tachyons exposing (classes)
import Views.Cards.StaticCard as CardView
import Views.Utils.Forms as Forms



-- TODO Waiting for Html.Extra to be updated


onClickStopPropagation : msg -> Html.Attribute msg
onClickStopPropagation msg =
    Html.Events.custom "click"
        (Decode.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = False
            }
        )


type alias Config msg =
    { createMsg : msg
    , onEditCard : Card.Model -> msg
    , onRemoveCard : Card.Model -> msg
    }


view : Config msg -> Array Card.Model -> Html msg
view { createMsg, onEditCard, onRemoveCard } cards =
    div []
        [ h2 [] [ text "My deck" ]
        , viewControls createMsg
        , viewCards onEditCard onRemoveCard cards
        ]


viewCards : (Card.Model -> msg) -> (Card.Model -> msg) -> Array Card.Model -> Html msg
viewCards onEditCard onRemoveCard cards =
    cards
        |> Array.map (\card -> ( card, CardView.view "300px" "600px" card ))
        |> Array.toList
        |> List.map
            (\( card, cardHtml ) ->
                div
                    [ classes
                        [ mr2
                        , mb2
                        , relative
                        , "card"
                        , pointer
                        ]
                    , style "zoom" "0.5"
                    , onClick (onEditCard card)
                    ]
                    [ cardHtml
                    , removeCross (onRemoveCard card)
                    ]
            )
        |> div [ classes [ flex, flex_wrap ] ]


removeCross : msg -> Html msg
removeCross onRemoveCard =
    span
        [ classes [ "remove", absolute, right_0, top_0, pa2, bg_white_60, f3, pointer, br1 ]
        , onClickStopPropagation onRemoveCard
        ]
        [ text "X" ]


viewControls : msg -> Html.Html msg
viewControls createMsg =
    div [ classes [ pv2 ] ] [ Forms.primaryButton [ onClick createMsg ] [ text "Create a new card" ] ]
