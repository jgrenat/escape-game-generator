module Pages.Deck exposing (Config, view)

import Array exposing (Array)
import Data.Card as Card
import Dict exposing (Dict)
import Html exposing (Html, button, div, h2, li, p, span, text, ul)
import Html.Attributes exposing (class, style)
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
        , viewDuplicateCardNumbers cards
        ]


viewDuplicateCardNumbers : Array Card.Model -> Html msg
viewDuplicateCardNumbers cards =
    cards
        |> Array.map Card.toNumber
        |> countNumberUses
        |> Dict.filter (\_ count -> count > 1)
        |> Dict.toList
        |> List.map viewSingleDuplicateCardNumber
        |> (\duplicateCardsList ->
                if List.length duplicateCardsList > 0 then
                    div [ class "duplicateCards" ]
                        [ p [] [ text "Be careful! You have some duplicate numbers amongst your cards:" ]
                        , ul [] duplicateCardsList
                        ]

                else
                    span [] []
           )


viewSingleDuplicateCardNumber : ( Int, Int ) -> Html msg
viewSingleDuplicateCardNumber ( number, count ) =
    li []
        [ text <| "Card number " ++ String.fromInt number ++ ": found " ++ String.fromInt count ++ " times"
        ]


countNumberUses : Array Int -> Dict Int Int
countNumberUses numbers =
    Array.foldl
        (\number dict ->
            case Dict.get number dict of
                Just count ->
                    Dict.insert number (count + 1) dict

                Nothing ->
                    Dict.insert number 1 dict
        )
        Dict.empty
        numbers


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
