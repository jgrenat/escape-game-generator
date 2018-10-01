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
    div
        ([ classes Styles.cardClasses ] ++ toStyle (Styles.cardStyles width height))
        [ viewIllustration model.illustration model.hiddenCards
        , viewCardContent model
        , viewNumber model.number
        ]


viewIllustration : Card.CardIllustration -> Array Card.HiddenCard -> Html msg
viewIllustration illustration hiddenCards =
    div
        [ style "width" "100%"
        , style "height" "60%"
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


viewCardContent : Card.Model -> Html msg
viewCardContent model =
    div
        ([ classes Styles.contentClasses ] ++ toStyle Styles.contentStyles)
        [ div [] [ Card.contentToString model.cardContent |> text ]
        ]


viewNumber : Int -> Html msg
viewNumber number =
    div
        ([ classes Styles.numberClasses ] ++ toStyle Styles.numberStyles)
        [ String.fromInt number |> text ]
