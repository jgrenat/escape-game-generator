module Views.Cards.StaticCard exposing (view, viewNumber)

import Array exposing (Array)
import CardEditor.Card as Card
import Html exposing (Attribute, Html, div, h1, img, small, span, text)
import Html.Attributes exposing (src, style)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (absolute, f4, o_10, cover, b)
import Views.Cards.CardStyles as Styles


view : String -> String -> Card.Model -> Html msg
view width height model =
    div
        [ style (Styles.cardStyles width height), classes Styles.cardClasses ]
        [ viewIllustration model.hiddenCards
        , viewCardContent model
        , viewNumber model.number
        ]


viewIllustration : Array Card.HiddenCard -> Html msg
viewIllustration hiddenCards =
    div
        [ style
            [ ( "width", "100%" )
            , ( "height", "60%" )
            , ( "background-image", "url('/assorted-business-cabinet-327173.jpg')" )
            , ( "border-bottom", "1px solid black" )
            ]
        , classes [ absolute, cover ]
        ]
        (Array.map viewHiddenCard hiddenCards |> Array.toList |> List.reverse)


viewHiddenCard : Card.HiddenCard -> Html msg
viewHiddenCard hiddenCard =
    div
        [ classes [ absolute, f4, o_10, b ]
        , Styles.hiddenCardStyles hiddenCard |> style
        ]
        [ text (toString hiddenCard.number) ]


viewCardContent : Card.Model -> Html msg
viewCardContent model =
    div
        [ style Styles.contentStyles
        , classes Styles.contentClasses
        ]
        [ div [] [ Card.contentToString model.cardContent |> text ]
        ]


viewNumber : Int -> Html msg
viewNumber number =
    div
        [ classes Styles.numberClasses
        , style Styles.numberStyles
        ]
        [ toString number |> text ]
