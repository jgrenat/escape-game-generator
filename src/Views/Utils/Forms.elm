module Views.Utils.Forms exposing (button, primaryButton, secondaryButton, submitButton)

import Html exposing (Html)
import Html.Attributes exposing (type_)
import Tachyons.Classes exposing (bg_black, bg_dark_blue, bg_dark_green, bg_light_gray, dib, dim, f6, link, mb2, ph3, pointer, pv2, white)
import Tachyons.Tachyons exposing (classes)


button : List (Html.Attribute msg) -> List (Html msg) -> Html msg
button attributes children =
    Html.button
        ([ type_ "button"
         , classes <| bg_black :: buttonClasses
         ]
            ++ attributes
        )
        children


submitButton : List (Html.Attribute msg) -> List (Html msg) -> Html msg
submitButton attributes children =
    Html.button
        ([ type_ "submit"
         , classes <| bg_dark_green :: buttonClasses
         ]
            ++ attributes
        )
        children


primaryButton : List (Html.Attribute msg) -> List (Html msg) -> Html msg
primaryButton attributes children =
    Html.button
        ([ type_ "button"
         , classes <| bg_dark_green :: buttonClasses
         ]
            ++ attributes
        )
        children


secondaryButton : List (Html.Attribute msg) -> List (Html msg) -> Html msg
secondaryButton attributes children =
    Html.button
        ([ type_ "button"
         , classes <| bg_dark_blue :: buttonClasses
         ]
            ++ attributes
        )
        children


buttonClasses : List String
buttonClasses =
    [ f6
    , link
    , dim
    , ph3
    , pv2
    , mb2
    , dib
    , white
    , pointer
    ]
