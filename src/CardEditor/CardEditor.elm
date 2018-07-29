port module CardEditor.CardEditor
    exposing
        ( Model
        , update
        , subscriptions
        , viewCard
        , viewStaticCard
        , viewCardController
        , Msg
        , createCard
        , Event(..)
        , encodeCard
        , cardDecoder
        )

import Array exposing (Array)
import CardEditor.Card as Card exposing (CardId, HiddenCard, contentToString, createCardCommand)
import Html exposing (Attribute, Html, button, div, fieldset, form, h1, img, input, label, legend, small, span, text, textarea)
import Html.Attributes exposing (contenteditable, for, id, name, src, step, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Mouse exposing (Position, position)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes exposing (absolute, b, b__black_10, b__black_20, ba, bg_gold, black_70, br2, br3, br4, br_100, cover, db, f3, f4, f6, flex, flex_row, fw6, h2, hover_black, items_center, justify_center, left_0, left_1, lh_copy, mb2, measure, ml1, ml2, ml3, ml5, mt2, mt3, o_10, o_20, o_50, overflow_hidden, pa2, pa3, ph1, ph2, pt1, pv2, relative, right_0, serif, top_0, top_1, w2, w5, w_100, w_50, w_80, w_90, white_20, white_40, white_50, white_70)


type Event
    = NoEvent
    | Save
    | Discard


type Model
    = NotCreated
    | Created Card.Model


cardDecoder : Decoder Model
cardDecoder =
    Card.decoder |> Decode.map Created


encodeCard : Model -> Maybe Decode.Value
encodeCard model =
    case model of
        NotCreated ->
            Nothing

        Created cardModel ->
            Card.encode cardModel |> Just


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
    = CardCreated Card.Model
    | UpdateField FieldUpdate
    | AddHiddenCard
    | RemoveHiddenCard Int
    | DragStart HiddenCard Card.DragStartDetails
    | DragAt Position
    | DragEnd (Result String Card.DragEndDetails)
    | DiscardChanges
    | SaveChanges


createCard : Card.Content -> ( Model, Cmd Msg )
createCard content =
    createCardCommand content CardCreated
        |> (\creationCommand -> ( NotCreated, creationCommand ))


update : Msg -> Model -> ( Model, Cmd Msg, Event )
update msg model =
    case ( msg, model ) of
        ( CardCreated cardModel, NotCreated ) ->
            ( Created cardModel, Cmd.none, NoEvent )

        ( _, Created cardModel ) ->
            updateCard msg cardModel
                |> (\( newModel, _, event ) -> ( Created newModel, Cmd.none, event ))

        _ ->
            ( model, Cmd.none, NoEvent )


updateCard : Msg -> Card.Model -> ( Card.Model, Cmd Msg, Event )
updateCard msg model =
    case msg of
        CardCreated _ ->
            ( model, Cmd.none, NoEvent )

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
                ( { model | draggedHiddenCard = Just drag }, Cmd.none, NoEvent )

        DragAt newPosition ->
            case model.draggedHiddenCard of
                Nothing ->
                    ( model, Cmd.none, NoEvent )

                Just dragDetails ->
                    let
                        offsetTop =
                            newPosition.y - dragDetails.initialPosition.y

                        offsetLeft =
                            newPosition.x - dragDetails.initialPosition.x

                        newDrag =
                            { dragDetails | currentTopOffset = offsetTop, currentLeftOffset = offsetLeft }
                    in
                        ( { model | draggedHiddenCard = Just newDrag }, Cmd.none, NoEvent )

        DragEnd (Ok dragEndDetails) ->
            case model.draggedHiddenCard of
                Nothing ->
                    ( model, Cmd.none, NoEvent )

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
                                model.hiddenCards
                    in
                        ( { model | draggedHiddenCard = Nothing, hiddenCards = hiddenCards }, Cmd.none, NoEvent )

        DragEnd (Err _) ->
            ( model, Cmd.none, NoEvent )

        AddHiddenCard ->
            let
                newHiddenCard =
                    defaultHiddenCard model.nextHiddenCardId

                newHiddenCardArray =
                    Array.push newHiddenCard model.hiddenCards
            in
                ( { model | hiddenCards = newHiddenCardArray, nextHiddenCardId = model.nextHiddenCardId + 1 }, Cmd.none, NoEvent )

        RemoveHiddenCard id ->
            let
                newHiddenCardsArray =
                    Array.filter (\hiddenCard -> hiddenCard.id /= id) model.hiddenCards
            in
                ( { model | hiddenCards = newHiddenCardsArray }, Cmd.none, NoEvent )

        UpdateField (CardContent newContent) ->
            ( { model | cardContent = Card.Content newContent }, Cmd.none, NoEvent )

        UpdateField (HiddenCardColor id newColor) ->
            let
                newModel =
                    updateHiddenCard model id (\hiddenCard -> { hiddenCard | color = newColor })
            in
                ( newModel, Cmd.none, NoEvent )

        UpdateField (HiddenCardNumber id newNumber) ->
            let
                newModel =
                    updateHiddenCard model
                        id
                        (\hiddenCard -> { hiddenCard | number = String.toInt newNumber |> Result.withDefault hiddenCard.number })
            in
                ( newModel, Cmd.none, NoEvent )

        UpdateField (HiddenCardTop id newValue) ->
            let
                newModel =
                    updateHiddenCard model
                        id
                        (\hiddenCard -> { hiddenCard | top = String.toFloat newValue |> Result.withDefault hiddenCard.top })
            in
                ( newModel, Cmd.none, NoEvent )

        UpdateField (HiddenCardOpacity id newValue) ->
            let
                newModel =
                    updateHiddenCard model
                        id
                        (\hiddenCard -> { hiddenCard | opacity = String.toFloat newValue |> Result.withDefault hiddenCard.opacity })
            in
                ( newModel, Cmd.none, NoEvent )

        UpdateField (HiddenCardSize id newValue) ->
            let
                newModel =
                    updateHiddenCard model
                        id
                        (\hiddenCard -> { hiddenCard | sizeInEm = String.toFloat newValue |> Result.withDefault hiddenCard.sizeInEm })
            in
                ( newModel, Cmd.none, NoEvent )

        UpdateField (HiddenCardRotation id newValue) ->
            let
                newModel =
                    updateHiddenCard model
                        id
                        (\hiddenCard -> { hiddenCard | rotation = String.toFloat newValue |> Result.withDefault hiddenCard.rotation })
            in
                ( newModel, Cmd.none, NoEvent )

        UpdateField (HiddenCardLeft id newValue) ->
            let
                newModel =
                    updateHiddenCard model
                        id
                        (\hiddenCard -> { hiddenCard | left = String.toFloat newValue |> Result.withDefault hiddenCard.left })
            in
                ( newModel, Cmd.none, NoEvent )

        DiscardChanges ->
            ( model, Cmd.none, Discard )

        SaveChanges ->
            ( model, Cmd.none, Save )


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
subscriptions model =
    case model of
        Created card ->
            case card.draggedHiddenCard of
                Nothing ->
                    Sub.none

                Just _ ->
                    Sub.batch [ Mouse.moves DragAt, mouseUp (Decode.decodeValue Card.dragEndDecoder >> DragEnd) ]

        _ ->
            Sub.none


port mouseUp : (Decode.Value -> msg) -> Sub msg



---- VIEW ----


{-| Deprecated method, should not be used there
-}
view : Model -> Html Msg
view model =
    case model of
        Created card ->
            div
                [ style [ ( "margin-left", "50px" ) ]
                , classes [ flex, flex_row ]
                ]
                [ tachyons.css
                , viewCard model
                , viewCardController model
                ]

        NotCreated ->
            div [] [ text "Creating card..." ]


viewStaticCard : String -> String -> Model -> Html msg
viewStaticCard width height model =
    case model of
        Created cardModel ->
            div
                [ style (cardStyles width height), classes cardClasses ]
                [ viewStaticIllustration cardModel.hiddenCards, viewStaticCardContent cardModel ]

        _ ->
            div [] [ text "Creating card..." ]


viewCard : Model -> Html Msg
viewCard model =
    case model of
        Created cardModel ->
            div
                [ style (cardStyles "300px" "600px"), classes cardClasses ]
                [ viewIllustration cardModel.draggedHiddenCard cardModel.hiddenCards
                , viewCardContent cardModel
                , viewNumber 66
                ]

        _ ->
            div [] [ text "Creating card..." ]


cardStyles : String -> String -> List ( String, String )
cardStyles width height =
    [ ( "width", width )
    , ( "height", height )
    , ( "border", "10px solid sienna" )
    ]


cardClasses : List String
cardClasses =
    [ overflow_hidden, br4, relative ]


viewStaticIllustration : Array HiddenCard -> Html msg
viewStaticIllustration hiddenCards =
    div
        [ style
            [ ( "width", "100%" )
            , ( "height", "60%" )
            , ( "background-image", "url('/assorted-business-cabinet-327173.jpg')" )
            , ( "border-bottom", "1px solid black" )
            ]
        , classes [ absolute, cover ]
        ]
        (Array.map viewStaticHiddenCard hiddenCards |> Array.toList |> List.reverse)


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
        [ style cardContentStyles
        , classes cardContentClasses
        ]
        [ div
            [ contenteditable True
            , on "blur" (Decode.map (CardContent >> UpdateField) targetTextContent)
            ]
            [ contentToString model.cardContent |> text ]
        ]


viewStaticCardContent : Card.Model -> Html msg
viewStaticCardContent model =
    div
        [ style cardContentStyles
        , classes cardContentClasses
        ]
        [ div [] [ contentToString model.cardContent |> text ]
        ]


viewNumber : Int -> Html msg
viewNumber number =
    div
        [ classes
            [ br_100
            , bg_gold
            , absolute
            , top_0
            , left_0
            , pa3
            ]
        ]
        [ toString number |> text ]


cardContentStyles : List ( String, String )
cardContentStyles =
    [ ( "width", "100%" )
    , ( "height", "40%" )
    , ( "top", "60%" )
    , ( "background-color", "cornsilk" )
    ]


cardContentClasses : List String
cardContentClasses =
    [ absolute, pa3, flex, justify_center, items_center, serif, f4 ]


targetTextContent : Decoder String
targetTextContent =
    Decode.at [ "target", "textContent" ] Decode.string



-- Deprecated


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
            , style (( "cursor", "move" ) :: hiddenCardStyles hiddenCard ++ transformStyles)
            , onMouseDown (DragStart hiddenCard)
            ]
            [ text (toString hiddenCard.number) ]


viewStaticHiddenCard : HiddenCard -> Html msg
viewStaticHiddenCard hiddenCard =
    div
        [ classes [ absolute, f4, o_10, b ]
        , hiddenCardStyles hiddenCard |> style
        ]
        [ text (toString hiddenCard.number) ]


hiddenCardStyles : HiddenCard -> List ( String, String )
hiddenCardStyles hiddenCard =
    [ ( "top", toString hiddenCard.top ++ "%" )
    , ( "left", toString hiddenCard.left ++ "%" )
    , ( "color", hiddenCard.color )
    , ( "font-size", toString hiddenCard.sizeInEm ++ "em" )
    , ( "opacity", toString hiddenCard.opacity )
    , ( "transform", "rotate(" ++ (toString hiddenCard.rotation) ++ "deg)" )
    , ( "user-select", "none" )
    ]


viewCardController : Model -> Html Msg
viewCardController model =
    case model of
        NotCreated ->
            div [] [ text "Creating card..." ]

        Created cardModel ->
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
