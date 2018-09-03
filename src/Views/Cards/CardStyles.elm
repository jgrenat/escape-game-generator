module Views.Cards.CardStyles exposing (cardClasses, cardStyles, contentClasses, contentStyles, hiddenCardStyles, numberClasses, numberStyles, toStyle)

import Data.Card as Card
import Html
import Html.Attributes exposing (style)
import Tachyons.Classes exposing (absolute, b, b__black_10, b__black_20, ba, bg_gold, black_70, br2, br3, br4, br_100, cover, db, f2, f3, f4, f6, flex, flex_row, fw6, h2, hover_black, items_center, justify_center, left_0, left_1, lh_copy, mb2, measure, ml1, ml2, ml3, ml5, mt2, mt3, o_10, o_20, o_50, o_60, o_90, overflow_hidden, pa2, pa3, ph1, ph2, pt1, pv2, relative, right_0, serif, top_0, top_1, w2, w5, w_100, w_20, w_50, w_80, w_90, white, white_20, white_40, white_50, white_70)


cardStyles : String -> String -> List ( String, String )
cardStyles width height =
    [ ( "width", width )
    , ( "height", height )
    , ( "border", "10px solid sienna" )
    ]


cardClasses : List String
cardClasses =
    [ overflow_hidden, br4, relative ]


contentStyles : List ( String, String )
contentStyles =
    [ ( "width", "100%" )
    , ( "height", "40%" )
    , ( "top", "60%" )
    , ( "background-color", "cornsilk" )
    ]


contentClasses : List String
contentClasses =
    [ absolute, pa3, flex, justify_center, items_center, serif, f4 ]


hiddenCardStyles : Card.HiddenCard -> List ( String, String )
hiddenCardStyles hiddenCard =
    [ ( "top", String.fromFloat hiddenCard.top ++ "%" )
    , ( "left", String.fromFloat hiddenCard.left ++ "%" )
    , ( "color", hiddenCard.color )
    , ( "font-size", String.fromFloat hiddenCard.sizeInEm ++ "em" )
    , ( "opacity", String.fromFloat hiddenCard.opacity )
    , ( "transform", "rotate(" ++ String.fromFloat hiddenCard.rotation ++ "deg)" )
    , ( "user-select", "none" )
    ]


numberClasses : List String
numberClasses =
    [ br_100
    , absolute
    , flex
    , justify_center
    , items_center
    , b
    , white
    , f3
    ]


numberStyles : List ( String, String )
numberStyles =
    [ ( "background-color", "sienna" )
    , ( "top", "-10px" )
    , ( "width", "20%" )
    , ( "height", "10%" )
    , ( "left", "-10px" )
    ]


toStyle : List ( String, String ) -> List (Html.Attribute msg)
toStyle styles =
    List.map (\( styleName, value ) -> style styleName value) styles
