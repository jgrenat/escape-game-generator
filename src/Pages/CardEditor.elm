port module Pages.CardEditor exposing
    ( Event(..)
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Array exposing (Array)
import Browser.Events
import Cropper
import Data.Card as Card exposing (CardId, CardIllustration, HiddenCard, contentToString, createCardCommand)
import Data.Drag as Drag exposing (Drag)
import Data.Position as Position exposing (Position)
import Html exposing (Attribute, Html, div, fieldset, form, h1, img, input, label, legend, small, span, text, textarea)
import Html.Attributes exposing (class, contenteditable, for, id, name, src, step, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Return
import Tachyons.Classes exposing (absolute, b, b__black_10, b__black_20, ba, bg_gold, black_70, br2, br3, br4, br_100, cover, db, f2, f3, f4, f6, flex, flex_row, fw6, h2, hover_black, items_center, justify_center, left_0, left_1, lh_copy, mb2, measure, ml1, ml2, ml3, ml5, mt2, mt3, o_10, o_20, o_50, o_60, o_90, overflow_hidden, pa2, pa3, ph1, ph2, pt1, pv2, relative, right_0, serif, top_0, top_1, w2, w5, w_100, w_20, w_50, w_80, w_90, white, white_20, white_40, white_50, white_70)
import Tachyons.Tachyons exposing (classes, tachyons)
import Views.Cards.CardStyles as CardStyles exposing (toStyle)
import Views.Cards.StaticCard as CardView
import Views.Utils.Forms as Forms


type Event
    = NoEvent
    | Save Card.Model
    | Discard


type Model
    = Model
        { cropper : Cropper
        , card : Card.Model
        , draggedHiddenCard : Maybe (Drag HiddenCard)
        }


type alias ImagePortData =
    { contents : String
    , filename : String
    }


type Cropper
    = NotCropping
    | Cropping Cropper.Model


init : Card.Model -> Model
init cardModel =
    Model { cropper = NotCropping, card = cardModel, draggedHiddenCard = Nothing }


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


type Msg
    = UpdateField FieldUpdate
    | AddHiddenCard
    | RemoveHiddenCard Int
    | DragStart HiddenCard Drag.DragStartDetails
    | DragAt Position.Position
    | DragEnd (Result Decode.Error Drag.DragEndDetails)
    | DiscardChanges
    | SaveChanges
    | IllustrationSelected
    | IllustrationCropped String
    | IllustrationRead ImagePortData
    | CropperMsg Cropper.Msg
    | ExportCroppedIllustration
    | Zoom String
    | CancelCropping


type FieldUpdate
    = CardContent String
    | CardNumber String
    | HiddenCardNumber Int String
    | HiddenCardColor Int String
    | HiddenCardTop Int String
    | HiddenCardLeft Int String
    | HiddenCardOpacity Int String
    | HiddenCardRotation Int String
    | HiddenCardSize Int String


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
            in
            ( Model { model | draggedHiddenCard = Just drag }, Cmd.none, NoEvent )

        DragAt newPosition ->
            case model.draggedHiddenCard of
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
                    in
                    ( Model { model | draggedHiddenCard = Just newDrag }, Cmd.none, NoEvent )

        DragEnd (Ok dragEndDetails) ->
            case model.draggedHiddenCard of
                Nothing ->
                    ( Model model, Cmd.none, NoEvent )

                Just draggedHiddenCard ->
                    let
                        newX =
                            toFloat (dragEndDetails.finalPosition.x - dragEndDetails.parentPosition.x - draggedHiddenCard.layerX)
                                / toFloat draggedHiddenCard.parentWidth
                                * 100
                                |> max 0
                                |> min 95

                        newY =
                            toFloat (dragEndDetails.finalPosition.y - dragEndDetails.parentPosition.y - draggedHiddenCard.layerY)
                                / toFloat draggedHiddenCard.parentHeight
                                * 100
                                |> max 0
                                |> min 95

                        hiddenCards =
                            updateIf
                                (\hiddenCard -> hiddenCard.id == draggedHiddenCard.element.id)
                                (\hiddenCard -> { hiddenCard | top = newY, left = newX })
                                card.hiddenCards

                        updatedCard =
                            { card | hiddenCards = hiddenCards }
                    in
                    ( Model { model | card = updatedCard, draggedHiddenCard = Nothing }, Cmd.none, NoEvent )

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

        UpdateField (CardNumber newNumber) ->
            let
                updatedCard =
                    { card | number = String.toInt newNumber |> Maybe.withDefault card.number }
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
                        (\hiddenCard -> { hiddenCard | number = String.toInt newNumber |> Maybe.withDefault hiddenCard.number })
            in
            ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

        UpdateField (HiddenCardTop id newValue) ->
            let
                updatedCard =
                    updateHiddenCard card
                        id
                        (\hiddenCard -> { hiddenCard | top = String.toFloat newValue |> Maybe.withDefault hiddenCard.top })
            in
            ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

        UpdateField (HiddenCardOpacity id newValue) ->
            let
                updatedCard =
                    updateHiddenCard card
                        id
                        (\hiddenCard -> { hiddenCard | opacity = String.toFloat newValue |> Maybe.withDefault hiddenCard.opacity })
            in
            ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

        UpdateField (HiddenCardSize id newValue) ->
            let
                updatedCard =
                    updateHiddenCard card
                        id
                        (\hiddenCard -> { hiddenCard | sizeInEm = String.toFloat newValue |> Maybe.withDefault hiddenCard.sizeInEm })
            in
            ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

        UpdateField (HiddenCardRotation id newValue) ->
            let
                updatedCard =
                    updateHiddenCard card
                        id
                        (\hiddenCard -> { hiddenCard | rotation = String.toFloat newValue |> Maybe.withDefault hiddenCard.rotation })
            in
            ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

        UpdateField (HiddenCardLeft id newValue) ->
            let
                updatedCard =
                    updateHiddenCard card
                        id
                        (\hiddenCard -> { hiddenCard | left = String.toFloat newValue |> Maybe.withDefault hiddenCard.left })
            in
            ( Model { model | card = updatedCard }, Cmd.none, NoEvent )

        DiscardChanges ->
            ( Model model, Cmd.none, Discard )

        SaveChanges ->
            ( Model model, Cmd.none, Save card )

        IllustrationSelected ->
            ( Model model, fileSelected "cardIllustrationInput", NoEvent )

        IllustrationRead illustrationDetails ->
            let
                updatedCard =
                    { card | illustration = Card.Base64 illustrationDetails.contents }

                updatedCropper =
                    Cropper.init { url = illustrationDetails.contents, crop = { width = 280, height = 348 } }
            in
            ( Model { model | card = updatedCard, cropper = Cropping updatedCropper }, Cmd.none, NoEvent )

        CropperMsg cropperMsg ->
            case model.cropper of
                NotCropping ->
                    ( Model model, Cmd.none, NoEvent )

                Cropping cropperModel ->
                    let
                        ( newCropper, cropperCmd ) =
                            Cropper.update cropperMsg cropperModel
                                |> Return.mapBoth CropperMsg Cropping
                    in
                    ( Model { model | cropper = newCropper }, cropperCmd, NoEvent )

        ExportCroppedIllustration ->
            case model.cropper of
                NotCropping ->
                    ( Model model, Cmd.none, NoEvent )

                Cropping cropperModel ->
                    ( Model model, toCropper (Cropper.cropData cropperModel), NoEvent )

        IllustrationCropped imageData ->
            case model.cropper of
                NotCropping ->
                    ( Model model, Cmd.none, NoEvent )

                Cropping cropperModel ->
                    let
                        updatedCard =
                            { card | illustration = Card.Base64 imageData }
                    in
                    ( Model { model | card = updatedCard, cropper = NotCropping }, Cmd.none, NoEvent )

        Zoom zoomString ->
            case ( model.cropper, String.toFloat zoomString ) of
                ( Cropping cropperModel, Just newZoom ) ->
                    let
                        newCropperModel =
                            Cropper.zoom cropperModel newZoom
                    in
                    ( Model { model | cropper = Cropping newCropperModel }, Cmd.none, NoEvent )

                _ ->
                    ( Model model, Cmd.none, NoEvent )

        CancelCropping ->
            ( Model { model | cropper = NotCropping }, Cmd.none, NoEvent )


updateHiddenCard : Card.Model -> Int -> (HiddenCard -> HiddenCard) -> Card.Model
updateHiddenCard cardModel hiddenCardId updateFunction =
    let
        newHiddenCards =
            updateIf
                (\hiddenCard -> hiddenCard.id == hiddenCardId)
                updateFunction
                cardModel.hiddenCards
    in
    { cardModel | hiddenCards = newHiddenCards }


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
subscriptions (Model { card, cropper, draggedHiddenCard }) =
    let
        dragSubscriptions =
            case draggedHiddenCard of
                Nothing ->
                    Sub.none

                Just _ ->
                    Sub.batch [ Browser.Events.onMouseMove (Position.decoder |> Decode.map DragAt), mouseUp (Decode.decodeValue Drag.dragEndDecoder >> DragEnd) ]

        cropperSubscriptions =
            case cropper of
                NotCropping ->
                    Sub.none

                Cropping cropperModel ->
                    Sub.batch [ Cropper.subscriptions cropperModel |> Sub.map CropperMsg, fromCropper IllustrationCropped ]
    in
    Sub.batch [ dragSubscriptions, fileContentRead IllustrationRead, cropperSubscriptions ]


port mouseUp : (Decode.Value -> msg) -> Sub msg


port fromCropper : (String -> msg) -> Sub msg


port toCropper : Cropper.CropData -> Cmd msg


port fileSelected : String -> Cmd msg


port fileContentRead : (ImagePortData -> msg) -> Sub msg



---- VIEW ----


view : Model -> Html Msg
view (Model { card, cropper, draggedHiddenCard }) =
    div []
        [ div
            [ style "margin-left" "50px"
            , classes [ flex, flex_row ]
            ]
            [ tachyons.css
            , viewCard card draggedHiddenCard
            , viewCardController card
            ]
        , viewCropper cropper
        ]


viewCard : Card.Model -> Maybe (Drag.Drag HiddenCard) -> Html Msg
viewCard cardModel draggedHiddenCard =
    div
        (classes CardStyles.cardClasses :: toStyle (CardStyles.cardStyles "300px" "600px"))
        [ viewIllustration cardModel.illustration draggedHiddenCard cardModel.hiddenCards
        , viewCardContent cardModel
        , CardView.viewNumber cardModel.number
        ]


viewIllustration : CardIllustration -> Maybe (Drag.Drag HiddenCard) -> Array HiddenCard -> Html Msg
viewIllustration illustration maybeDraggedHiddenCard hiddenCards =
    div
        [ style "width" "100%"
        , style "height" "60%"
        , CardStyles.illustrationToBackgroundStyle illustration
        , style "border-bottom" "1px solid black"
        , id "js-area"
        , classes [ absolute, cover ]
        ]
        (Array.map (viewHiddenCard maybeDraggedHiddenCard) hiddenCards |> Array.toList |> List.reverse)


viewCardContent : Card.Model -> Html Msg
viewCardContent model =
    div
        ([ classes CardStyles.contentClasses ] ++ toStyle CardStyles.contentStyles)
        [ div
            [ contenteditable True
            , on "blur" (Decode.map (CardContent >> UpdateField) targetTextContent)
            ]
            [ contentToString model.cardContent |> text ]
        ]


targetTextContent : Decoder String
targetTextContent =
    Decode.at [ "target", "textContent" ] Decode.string


viewHiddenCard : Maybe (Drag.Drag HiddenCard) -> HiddenCard -> Html Msg
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
                                "translateX(" ++ String.fromInt draggedHiddenCard.currentLeftOffset ++ "px)"

                            translateY =
                                "translateY(" ++ String.fromInt draggedHiddenCard.currentTopOffset ++ "px)"

                            rotate =
                                "rotate(" ++ String.fromFloat draggedHiddenCard.element.rotation ++ "deg)"
                        in
                        [ ( "transform", translateX ++ " " ++ translateY ++ " " ++ rotate ) ]
    in
    div
        ([ classes [ absolute, f4, o_10, b ]
         , onMouseDown (DragStart hiddenCard)
         ]
            ++ (style "cursor" "move" :: toStyle (CardStyles.hiddenCardStyles hiddenCard))
            ++ toStyle transformStyles
        )
        [ text (String.fromInt hiddenCard.number) ]


viewStaticHiddenCard : HiddenCard -> Html msg
viewStaticHiddenCard hiddenCard =
    div
        ([ classes [ absolute, f4, o_10, b ] ] ++ toStyle (CardStyles.hiddenCardStyles hiddenCard))
        [ text (String.fromInt hiddenCard.number) ]


viewCardController : Card.Model -> Html Msg
viewCardController cardModel =
    form [ style "width" "300px", classes [ ml5 ] ]
        [ cardIllustrationInput
        , cardNumberInput cardModel.number
        , cardContentTextarea (contentToString cardModel.cardContent)
        , div [] (Array.map viewHiddenCardForm cardModel.hiddenCards |> Array.toList)
        , div []
            [ Forms.secondaryButton [ onClick AddHiddenCard ] [ text "Add another hidden card" ]
            ]
        , div []
            [ Forms.button [ type_ "button", onClick DiscardChanges ] [ text "Cancel changes" ]
            , Forms.primaryButton [ type_ "button", onClick SaveChanges ] [ text "Save changes" ]
            ]
        ]


cardIllustrationInput : Html Msg
cardIllustrationInput =
    div [ classes [ mt3 ] ]
        [ label [ for "cardIllustrationInput", classes [ f6, db, mb2 ] ] [ text "Card illustration" ]
        , input
            [ id "cardIllustrationInput"
            , name "cardIllustrationInput"
            , type_ "file"
            , on "change" (Decode.succeed IllustrationSelected)
            , classes [ db, hover_black, w_100, measure, ba, b__black_20, pa2, br2, mb2 ]
            ]
            []
        ]


cardNumberInput : Int -> Html Msg
cardNumberInput number =
    div [ classes [ mt3 ] ]
        [ label [ for "numberInput", classes [ f6, db, mb2 ] ] [ text "Card number" ]
        , input
            [ id "numberInput"
            , name "numberInput"
            , type_ "number"
            , step "1"
            , value (String.fromInt number)
            , onInput (CardNumber >> UpdateField)
            , classes [ db, hover_black, w_100, measure, ba, b__black_20, pa2, br2, mb2 ]
            ]
            []
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
            String.fromInt hiddenCard.id
    in
    fieldset [ classes [ db, hover_black, w_100, ba, b__black_20, pa2, pt1, br2, mb2, mt3, relative ] ]
        [ legend [ classes [ ph1 ] ] [ text <| "Hidden Card " ++ stringId ]
        , Forms.button [ classes [ absolute, top_0, right_0, db ], onClick (RemoveHiddenCard hiddenCard.id) ] [ text "X" ]
        , div [ classes [ flex ] ]
            [ div
                [ classes [ mt3 ]
                ]
                [ label [ for <| "hiddenCardNumberInput" ++ stringId, classes [ f6, db, mb2 ] ] [ text "Card number" ]
                , input
                    [ id <| "hiddenCardNumberInput" ++ stringId
                    , name <| "hiddenCardNumberInput" ++ stringId
                    , type_ "number"
                    , value (String.fromInt hiddenCard.number)
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
                    , value <| String.fromFloat hiddenCard.sizeInEm
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
                    , value (String.fromFloat hiddenCard.rotation)
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
                    , value <| String.fromFloat hiddenCard.opacity
                    , onInput (HiddenCardOpacity hiddenCard.id >> UpdateField)
                    , classes [ db, hover_black, measure, pv2, br2, mb2, w_80 ]
                    ]
                    []
                ]
            ]
        ]


viewCropper : Cropper -> Html Msg
viewCropper cropper =
    case cropper of
        NotCropping ->
            span [] []

        Cropping cropperModel ->
            div [ class "cropperOverlay" ]
                [ viewCropperPopin cropperModel
                ]


viewCropperPopin : Cropper.Model -> Html Msg
viewCropperPopin cropperModel =
    div [ class "cropperPopin" ]
        [ div [ style "flex-grow" "1" ] [ Cropper.view cropperModel |> Html.map CropperMsg ]
        , viewCropperControls cropperModel
        ]


viewCropperControls : Cropper.Model -> Html Msg
viewCropperControls cropperModel =
    div []
        [ div
            [ classes [ mt3 ] ]
            [ label [ for <| "zoom", classes [ f6, db, mb2 ] ] [ text "Zoom" ]
            , input
                [ id "zoom"
                , name "zoom"
                , type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "1"
                , Html.Attributes.step "0.05"
                , value <| String.fromFloat cropperModel.zoom
                , onInput Zoom
                , classes [ db, hover_black, measure, pv2, br2, mb2, w_80 ]
                ]
                []
            ]
        , Forms.secondaryButton [ onClick CancelCropping ] [ text "Cancel" ]
        , Forms.submitButton [ onClick ExportCroppedIllustration ] [ text "Use this image" ]
        ]


onMouseDown : (Drag.DragStartDetails -> Msg) -> Attribute Msg
onMouseDown eventBuilder =
    Decode.map5
        Drag.DragStartDetails
        Position.decoder
        (Decode.at [ "target", "parentElement", "offsetWidth" ] (Decode.map round Decode.float))
        (Decode.at [ "target", "parentElement", "offsetHeight" ] (Decode.map round Decode.float))
        (Decode.field "layerX" Decode.int)
        (Decode.field "layerY" Decode.int)
        |> Decode.map eventBuilder
        |> on "mousedown"
