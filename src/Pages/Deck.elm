module Pages.Deck exposing (Config, view)

import Array exposing (Array)
import Data.Card as Card
import Dict exposing (Dict)
import Html exposing (Html, a, div, h2, input, li, p, span, text, ul)
import Html.Attributes exposing (class, id, placeholder, style, type_)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Route exposing (href)
import Tachyons.Classes exposing (absolute, bg_white_60, br1, f3, flex, flex_wrap, mb2, mr2, pa2, pointer, pv2, relative, right_0, top_0)
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
    { createMsg : Card.CardType -> msg
    , exportDeck : msg
    , importDeck : msg
    , onEditCard : Card.Model -> msg
    , onRemoveCard : Card.Model -> msg
    }


view : Config msg -> String -> Array Card.Model -> Html msg
view { createMsg, exportDeck, importDeck, onEditCard, onRemoveCard } deckId cards =
    div []
        [ h2 [] [ text "My deck" ]
        , viewControls deckId createMsg exportDeck importDeck
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


viewControls : String -> (Card.CardType -> msg) -> msg -> msg -> Html.Html msg
viewControls deckId createMsg exportDeck importDeck =
    div [ classes [ pv2 ] ]
        [ div []
            [ Forms.primaryButton [ onClick (createMsg Card.IllustrationAndText), classes [ mr2 ] ] [ text "New \"Illustration and text\" card" ]
            , Forms.primaryButton [ onClick (createMsg Card.FullIllustration), classes [ mr2 ] ] [ text "New \"Full illustration\" card" ]
            , Forms.primaryButton [ onClick exportDeck ] [ text "Export deck" ]
            , a [ href (Route.PrintDeck deckId) ] [ text "Print mode" ]
            ]
        , input
            [ type_ "file"
            , id "deckImportInput"
            , placeholder "Import deck"
            , classes [ mb2 ]
            , on "change" (Decode.succeed importDeck)
            ]
            []
        ]
