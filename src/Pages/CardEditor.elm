port module Pages.CardEditor
    exposing
        ( Model
        , update
        , subscriptions
        , view
        , Msg
        , Event(..)
        , init
        )

import Array exposing (Array)
import Views.Cards.CardStyles as CardStyles
import Views.Cards.StaticCard as CardView
import Data.Card as Card exposing (CardId, HiddenCard, contentToString, createCardCommand)
import Html exposing (Attribute, Html, button, div, fieldset, form, h1, img, input, label, legend, small, span, text, textarea)
import Html.Attributes exposing (contenteditable, for, id, name, src, step, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Mouse exposing (Position, position)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes exposing (absolute, b, b__black_10, b__black_20, ba, bg_gold, black_70, br2, br3, br4, br_100, cover, db, f2, f3, f4, f6, flex, flex_row, fw6, h2, hover_black, items_center, justify_center, left_0, left_1, lh_copy, mb2, measure, ml1, ml2, ml3, ml5, mt2, mt3, o_10, o_20, o_50, o_60, o_90, overflow_hidden, pa2, pa3, ph1, ph2, pt1, pv2, relative, right_0, serif, top_0, top_1, w2, w5, w_100, w_20, w_50, w_80, w_90, white, white_20, white_40, white_50, white_70)


type Event
    = NoEvent
    | Save Card.Model
    | Discard


type Model
    = Model
        { card : Card.Model
        }


init : Card.Model -> Model
init cardModel =
    Model { card = cardModel }


defaultHiddenCard : Int -> HiddenCard
defaultHiddenCard id =
    { id = id
    , number = 1
    , color = "#ffffff"
    , sizeInEm = 1
    , top = 50
    , left = 50
    , opacity = 1
    , rotation = 0
    }



---- UPDATE ----


type FieldUpdate
    = CardContent String
    | HiddenCardNumber Int String
    | HiddenCardColor Int String
    | HiddenCardTop Int String
    | HiddenCardLeft Int String
    | HiddenCardOpacity Int String
    | HiddenCardRotation Int String
    | HiddenCardSize Int String


type Msg
    = UpdateField FieldUpdate
    | AddHiddenCard
    | RemoveHiddenCard Int
    | DragStart HiddenCard Card.DragStartDetails
    | DragAt Position
    | DragEnd (Result String Card.DragEndDetails)
    | DiscardChanges
    | SaveChanges


update : Msg -> Model -> ( Model, Cmd Msg, Event )
update msg (Model model) =
    let
        card =
            model.card
    in
        case msg of
            DragStart hiddenCard dragStartDetails ->
                let
                    drag =
                        { element = hiddenCard
                        , currentTopOffset = 0
                        , currentLeftOffset = 0
                        , initialPosition = dragStartDetails.initialPosition
                        , parentWidth = dragStartDetails.parentWidth
                        , parentHeight = dragStartDetails.parentHeight
                        , layerX = dragStartDetails.layerX
                        , layerY = dragStartDetails.layerY
                        }

                    updatedCard =
                        { card | draggedHiddenCard = Just drag }
                in
                    ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            DragAt newPosition ->
                case card.draggedHiddenCard of
                    Nothing ->
                        ( Model model, Cmd.none, NoEvent )

                    Just dragDetails ->
                        let
                            offsetTop =
                                newPosition.y - dragDetails.initialPosition.y

                            offsetLeft =
                                newPosition.x - dragDetails.initialPosition.x

                            newDrag =
                                { dragDetails | currentTopOffset = offsetTop, currentLeftOffset = offsetLeft }

                            updatedCard =
                                { card | draggedHiddenCard = Just newDrag }
                        in
                            ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            DragEnd (Ok dragEndDetails) ->
                case card.draggedHiddenCard of
                    Nothing ->
                        ( Model model, Cmd.none, NoEvent )

                    Just draggedHiddenCard ->
                        let
                            newX =
                                (toFloat (dragEndDetails.finalPosition.x - dragEndDetails.parentPosition.x - draggedHiddenCard.layerX))
                                    / (toFloat draggedHiddenCard.parentWidth)
                                    * 100
                                    |> max 0
                                    |> min 95

                            newY =
                                (toFloat (dragEndDetails.finalPosition.y - dragEndDetails.parentPosition.y - draggedHiddenCard.layerY))
                                    / (toFloat draggedHiddenCard.parentHeight)
                                    * 100
                                    |> max 0
                                    |> min 95

                            hiddenCards =
                                updateIf
                                    (\hiddenCard -> hiddenCard.id == draggedHiddenCard.element.id)
                                    (\hiddenCard -> { hiddenCard | top = newY, left = newX })
                                    card.hiddenCards

                            updatedCard =
                                { card | draggedHiddenCard = Nothing, hiddenCards = hiddenCards }
                        in
                            ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            DragEnd (Err _) ->
                ( Model model, Cmd.none, NoEvent )

            AddHiddenCard ->
                let
                    newHiddenCard =
                        defaultHiddenCard card.nextHiddenCardId

                    newHiddenCardArray =
                        Array.push newHiddenCard card.hiddenCards

                    updatedCard =
                        { card | hiddenCards = newHiddenCardArray, nextHiddenCardId = card.nextHiddenCardId + 1 }
                in
                    ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            RemoveHiddenCard id ->
                let
                    newHiddenCardsArray =
                        Array.filter (\hiddenCard -> hiddenCard.id /= id) card.hiddenCards

                    updatedCard =
                        { card | hiddenCards = newHiddenCardsArray }
                in
                    ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            UpdateField (CardContent newContent) ->
                let
                    updatedCard =
                        { card | cardContent = Card.Content newContent }
                in
                    ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            UpdateField (HiddenCardColor id newColor) ->
                let
                    updatedCard =
                        updateHiddenCard card id (\hiddenCard -> { hiddenCard | color = newColor })
                in
                    ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            UpdateField (HiddenCardNumber id newNumber) ->
                let
                    updatedCard =
                        updateHiddenCard card
                            id
                            (\hiddenCard -> { hiddenCard | number = String.toInt newNumber |> Result.withDefault hiddenCard.number })
                in
                    ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            UpdateField (HiddenCardTop id newValue) ->
                let
                    updatedCard =
                        updateHiddenCard card
                            id
                            (\hiddenCard -> { hiddenCard | top = String.toFloat newValue |> Result.withDefault hiddenCard.top })
                in
                    ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            UpdateField (HiddenCardOpacity id newValue) ->
                let
                    updatedCard =
                        updateHiddenCard card
                            id
                            (\hiddenCard -> { hiddenCard | opacity = String.toFloat newValue |> Result.withDefault hiddenCard.opacity })
                in
                    ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            UpdateField (HiddenCardSize id newValue) ->
                let
                    updatedCard =
                        updateHiddenCard card
                            id
                            (\hiddenCard -> { hiddenCard | sizeInEm = String.toFloat newValue |> Result.withDefault hiddenCard.sizeInEm })
                in
                    ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            UpdateField (HiddenCardRotation id newValue) ->
                let
                    updatedCard =
                        updateHiddenCard card
                            id
                            (\hiddenCard -> { hiddenCard | rotation = String.toFloat newValue |> Result.withDefault hiddenCard.rotation })
                in
                    ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            UpdateField (HiddenCardLeft id newValue) ->
                let
                    updatedCard =
                        updateHiddenCard card
                            id
                            (\hiddenCard -> { hiddenCard | left = String.toFloat newValue |> Result.withDefault hiddenCard.left })
                in
                    ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

            DiscardChanges ->
                ( Model model, Cmd.none, Discard )

            SaveChanges ->
                ( Model model, Cmd.none, Save card )


updateHiddenCard : Card.Model -> Int -> (HiddenCard -> HiddenCard) -> Card.Model
updateHiddenCard cardModel hiddenCardId updateFunction =
    let
        newHiddenCards =
            updateIf
                (\hiddenCard -> hiddenCard.id == hiddenCardId)
                updateFunction
                cardModel.hiddenCards
    in
        ({ cardModel | hiddenCards = newHiddenCards })


updateIf : (a -> Bool) -> (a -> a) -> Array a -> Array a
updateIf filter transform elements =
    Array.map
        (\element ->
            if filter element then
                transform element
            else
                element
        )
        elements



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions (Model { card }) =
    case card.draggedHiddenCard of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, mouseUp (Decode.decodeValue Card.dragEndDecoder >> DragEnd) ]


port mouseUp : (Decode.Value -> msg) -> Sub msg



---- VIEW ----


view : Model -> Html Msg
view (Model { card }) =
    div
        [ style [ ( "margin-left", "50px" ) ]
        , classes [ flex, flex_row ]
        ]
        [ tachyons.css
        , viewCard card
        , viewCardController card
        ]


viewCard : Card.Model -> Html Msg
viewCard cardModel =
    div
        [ style (CardStyles.cardStyles "300px" "600px"), classes CardStyles.cardClasses ]
        [ viewIllustration cardModel.draggedHiddenCard cardModel.hiddenCards
        , viewCardContent cardModel
        , CardView.viewNumber cardModel.number
        ]


viewIllustration : Maybe (Card.Drag HiddenCard) -> Array HiddenCard -> Html Msg
viewIllustration maybeDraggedHiddenCard hiddenCards =
    div
        [ style
            [ ( "width", "100%" )
            , ( "height", "60%" )
            , ( "background-image", "url('/assorted-business-cabinet-327173.jpg')" )
            , ( "border-bottom", "1px solid black" )
            ]
        , id "js-area"
        , classes [ absolute, cover ]
        ]
        (Array.map (viewHiddenCard maybeDraggedHiddenCard) hiddenCards |> Array.toList |> List.reverse)


viewCardContent : Card.Model -> Html Msg
viewCardContent model =
    div
        [ style CardStyles.contentStyles
        , classes CardStyles.contentClasses
        ]
        [ div
            [ contenteditable True
            , on "blur" (Decode.map (CardContent >> UpdateField) targetTextContent)
            ]
            [ contentToString model.cardContent |> text ]
        ]


targetTextContent : Decoder String
targetTextContent =
    Decode.at [ "target", "textContent" ] Decode.string


viewHiddenCard : Maybe (Card.Drag HiddenCard) -> HiddenCard -> Html Msg
viewHiddenCard maybeHiddenCard hiddenCard =
    let
        transformStyles =
            case maybeHiddenCard of
                Nothing ->
                    []

                Just draggedHiddenCard ->
                    if draggedHiddenCard.element.id /= hiddenCard.id then
                        []
                    else
                        let
                            translateX =
                                "translateX(" ++ (toString draggedHiddenCard.currentLeftOffset) ++ "px)"

                            translateY =
                                "translateY(" ++ (toString draggedHiddenCard.currentTopOffset) ++ "px)"

                            rotate =
                                "rotate(" ++ (toString draggedHiddenCard.element.rotation) ++ "deg)"
                        in
                            [ ( "transform", translateX ++ " " ++ translateY ++ " " ++ rotate ) ]
    in
        div
            [ classes [ absolute, f4, o_10, b ]
            , style (( "cursor", "move" ) :: CardStyles.hiddenCardStyles hiddenCard ++ transformStyles)
            , onMouseDown (DragStart hiddenCard)
            ]
            [ text (toString hiddenCard.number) ]


viewStaticHiddenCard : HiddenCard -> Html msg
viewStaticHiddenCard hiddenCard =
    div
        [ classes [ absolute, f4, o_10, b ]
        , CardStyles.hiddenCardStyles hiddenCard |> style
        ]
        [ text (toString hiddenCard.number) ]


viewCardController : Card.Model -> Html Msg
viewCardController cardModel =
    form [ style [ ( "width", "300px" ) ], classes [ ml5 ] ]
        [ cardContentTextarea (contentToString cardModel.cardContent)
        , div [] (Array.map viewHiddenCardForm cardModel.hiddenCards |> Array.toList)
        , div []
            [ button [ type_ "button", onClick AddHiddenCard ] [ text "Add another hidden card" ]
            ]
        , div []
            [ button [ type_ "button", onClick DiscardChanges ] [ text "Cancel changes" ]
            , button [ type_ "button", onClick SaveChanges ] [ text "Save changes" ]
            ]
        ]


cardContentTextarea : String -> Html Msg
cardContentTextarea content =
    div [ classes [ mt3 ] ]
        [ label [ for "cardContentTextarea", classes [ f6, db, mb2 ] ] [ text "Content of the card" ]
        , textarea
            [ id "cardContentTextarea"
            , name "cardContentTextarea"
            , classes [ db, hover_black, w_100, measure, ba, b__black_20, pa2, br2, mb2 ]
            , onInput (CardContent >> UpdateField)
            ]
            [ text content ]
        ]


viewHiddenCardForm : HiddenCard -> Html Msg
viewHiddenCardForm hiddenCard =
    let
        stringId =
            toString hiddenCard.id
    in
        fieldset [ classes [ db, hover_black, w_100, ba, b__black_20, pa2, pt1, br2, mb2, mt3, relative ] ]
            [ legend [ classes [ ph1 ] ] [ text <| "Hidden Card " ++ stringId ]
            , button [ classes [ absolute, top_0, right_0, db ], type_ "button", onClick (RemoveHiddenCard hiddenCard.id) ] [ text "X" ]
            , div [ classes [ flex ] ]
                [ div
                    [ classes [ mt3 ]
                    ]
                    [ label [ for <| "hiddenCardNumberInput" ++ stringId, classes [ f6, db, mb2 ] ] [ text "Card number" ]
                    , input
                        [ id <| "hiddenCardNumberInput" ++ stringId
                        , name <| "hiddenCardNumberInput" ++ stringId
                        , type_ "number"
                        , value (toString hiddenCard.number)
                        , onInput (HiddenCardNumber hiddenCard.id >> UpdateField)
                        , classes [ db, hover_black, w_100, measure, ba, b__black_20, pa2, br2, mb2 ]
                        ]
                        []
                    ]
                , div
                    [ classes [ mt3, ml5, w_50 ]
                    ]
                    [ label [ for <| "hiddenCardSizeInput" ++ stringId, classes [ f6, db, mb2 ] ]
                        [ text "Size" ]
                    , input
                        [ id <| "hiddenCardSizeInput" ++ stringId
                        , name <| "hiddenCardSizeInput" ++ stringId
                        , type_ "range"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "5"
                        , Html.Attributes.step "0.1"
                        , value <| toString hiddenCard.sizeInEm
                        , onInput (HiddenCardSize hiddenCard.id >> UpdateField)
                        , classes [ db, hover_black, measure, pv2, br2, mb2, w_80 ]
                        ]
                        []
                    ]
                ]
            , div [ classes [ flex ] ]
                [ div
                    [ classes [ mt3, w_50 ]
                    ]
                    [ label [ for <| "hiddenCardColorInput" ++ stringId, classes [ f6, db, mb2 ] ]
                        [ text "Text color" ]
                    , input
                        [ id <| "hiddenCardColorInput" ++ stringId
                        , name <| "hiddenCardColorInput" ++ stringId
                        , type_ "color"
                        , value hiddenCard.color
                        , onInput (HiddenCardColor hiddenCard.id >> UpdateField)
                        , classes [ db, hover_black, measure, br2, mb2 ]
                        ]
                        []
                    ]
                , div
                    [ classes [ mt3, w_50 ]
                    ]
                    [ label [ for <| "hiddenCardRotationInput" ++ stringId, classes [ f6, db, mb2 ] ]
                        [ text "Rotation" ]
                    , input
                        [ id <| "hiddenCardRotationInput" ++ stringId
                        , name <| "hiddenCardRotationInput" ++ stringId
                        , type_ "number"
                        , Html.Attributes.min "-360"
                        , Html.Attributes.max "360"
                        , step "5"
                        , value (toString hiddenCard.rotation)
                        , onInput (HiddenCardRotation hiddenCard.id >> UpdateField)
                        , classes [ db, hover_black, measure, br2, mb2 ]
                        ]
                        []
                    ]
                , div
                    [ classes [ mt3, w_50 ]
                    ]
                    [ label [ for <| "hiddenCardOpacityInput" ++ stringId, classes [ f6, db, mb2 ] ]
                        [ text "Opacity" ]
                    , input
                        [ id <| "hiddenCardOpacityInput" ++ stringId
                        , name <| "hiddenCardOpacityInput" ++ stringId
                        , type_ "range"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "1"
                        , Html.Attributes.step "0.1"
                        , value <| toString hiddenCard.opacity
                        , onInput (HiddenCardOpacity hiddenCard.id >> UpdateField)
                        , classes [ db, hover_black, measure, pv2, br2, mb2, w_80 ]
                        ]
                        []
                    ]
                ]
            ]


onMouseDown : (Card.DragStartDetails -> Msg) -> Attribute Msg
onMouseDown eventBuilder =
    Decode.map5
        Card.DragStartDetails
        position
        (Decode.at [ "target", "parentElement", "offsetWidth" ] Decode.int)
        (Decode.at [ "target", "parentElement", "offsetHeight" ] Decode.int)
        (Decode.field "layerX" Decode.int)
        (Decode.field "layerY" Decode.int)
        |> Decode.map eventBuilder
        |> on "mousedown"
