module Views.Cards.StaticCard exposing (view, viewNumber)

import Array exposing (Array)
import Data.Card as Card
import Html exposing (Attribute, Html, div, h1, img, small, span, text)
import Html.Attributes exposing (src, style)
import Tachyons.Classes exposing (absolute, b, cover, f4, o_10)
import Tachyons.Tachyons exposing (classes)
import Views.Cards.CardStyles as Styles exposing (toStyle)


view : String -> String -> Card.Model -> Html msg
view width height model =
    let
        cardModel =
            Card.toCardModel model

        cardTypeDetails =
            Card.toCardTypeDetails model
    in
    case cardTypeDetails of
        Card.IllustrationAndTextCardDetails details ->
            div
                ([ classes Styles.cardClasses ] ++ toStyle (Styles.cardStyles width height))
                [ viewHalfCardIllustration cardModel.illustration cardModel.hiddenCards
                , viewCardContent details.cardContent
                , viewNumber cardModel.number
                ]

        Card.FullIllustrationCardDetails ->
            div ([ classes Styles.cardClasses ] ++ toStyle (Styles.cardStyles width height))
                [ viewFullSizeIllustration cardModel.illustration cardModel.hiddenCards
                , viewNumber cardModel.number
                ]


viewHalfCardIllustration : Card.CardIllustration -> Array Card.HiddenCard -> Html msg
viewHalfCardIllustration illustration hiddenCards =
    div
        [ style "width" "100%"
        , style "height" "60%"
        , Styles.illustrationToBackgroundStyle illustration
        , style "border-bottom" "1px solid black"
        , classes [ absolute, cover ]
        ]
        (Array.map viewHiddenCard hiddenCards |> Array.toList |> List.reverse)


viewFullSizeIllustration : Card.CardIllustration -> Array Card.HiddenCard -> Html msg
viewFullSizeIllustration illustration hiddenCards =
    div
        [ style "width" "100%"
        , style "height" "100%"
        , Styles.illustrationToBackgroundStyle illustration
        , style "border-bottom" "1px solid black"
        , classes [ absolute, cover ]
        ]
        (Array.map viewHiddenCard hiddenCards |> Array.toList |> List.reverse)


viewHiddenCard : Card.HiddenCard -> Html msg
viewHiddenCard hiddenCard =
    div
        ([ classes [ absolute, f4, o_10, b ] ] ++ toStyle (Styles.hiddenCardStyles hiddenCard))
        [ text (String.fromInt hiddenCard.number) ]


viewCardContent : Card.Content -> Html msg
viewCardContent content =
    div
        ([ classes Styles.contentClasses ] ++ toStyle Styles.contentStyles)
        [ div [] [ Card.contentToString content |> text ]
        ]


viewNumber : Int -> Html msg
viewNumber number =
    div
        ([ classes Styles.numberClasses ] ++ toStyle Styles.numberStyles)
        [ String.fromInt number |> text ]
