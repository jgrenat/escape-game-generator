port module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import CardEditor.CardEditor as CardEditor exposing (Event(Discard), createCard, encodeCard, viewCard, viewCardController)
import CardEditor.Card exposing (Content(Content))
import Html exposing (div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode exposing (Value)
import Json.Decode.Pipeline as JsonPipeline
import Json.Encode
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes exposing (flex, flex_row, mr0, mr1, mr2, mt2, z_0)
import Time


main : Program Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Value -> ( Model, Cmd Msg )
init savedGame =
    let
        decodedModelResult =
            Json.Decode.decodeValue Json.Decode.string savedGame
                |> Result.andThen (Json.Decode.decodeString modelDecoder)
    in
        case decodedModelResult of
            Ok model ->
                ( model, Cmd.none )

            Err _ ->
                newGameSet


newGameSet : ( Model, Cmd Msg )
newGameSet =
    let
        ( cardModel, cardCommand ) =
            createCard (Content "Random content")

        ( cardModel2, cardCommand2 ) =
            createCard (Content "Random content")
    in
        ( (Array.fromList [ cardModel, cardModel, cardModel, cardModel ] |> Model) AutoSave NoCard
        , Cmd.batch
            [ Cmd.map (CardMessage cardModel) cardCommand
            , Cmd.map (CardMessage cardModel2) cardCommand2
            , Cmd.map (CardMessage cardModel2) cardCommand2
            , Cmd.map (CardMessage cardModel2) cardCommand2
            ]
        )



-- MODEL


type alias Model =
    { cards : Array CardEditor.Model
    , saveOption : SaveOption
    , cardBeingEdited : CardBeingEdited
    }


type SaveOption
    = AutoSave
    | ManualOnly


type CardBeingEdited
    = NoCard
    | Card CardEditor.Model CardEditor.Model


modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    JsonPipeline.decode Model
        |> JsonPipeline.required "cards" cardsArrayDecoder
        |> JsonPipeline.required "autosave" autoSaveDecoder
        |> JsonPipeline.hardcoded NoCard


cardsArrayDecoder : Json.Decode.Decoder (Array CardEditor.Model)
cardsArrayDecoder =
    Json.Decode.array CardEditor.cardDecoder


autoSaveDecoder : Json.Decode.Decoder SaveOption
autoSaveDecoder =
    Json.Decode.bool
        |> Json.Decode.map
            (\isAutoSave ->
                if isAutoSave then
                    AutoSave
                else
                    ManualOnly
            )



-- UPDATE


type Msg
    = CardMessage CardEditor.Model CardEditor.Msg
    | CardBeingEditedMessage CardEditor.Msg
    | EditCard CardEditor.Model
    | SaveDeck


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CardMessage cardModel cardMessage ->
            let
                ( updatedCard, command, _ ) =
                    updateCard cardMessage cardModel

                updatedCards =
                    replaceCardInArray cardModel updatedCard model.cards
            in
                ( { model | cards = updatedCards }, command )

        CardBeingEditedMessage cardMessage ->
            case model.cardBeingEdited of
                NoCard ->
                    ( model, Cmd.none )

                Card previousCard changedCard ->
                    let
                        ( updatedCard, command, event ) =
                            updateCard cardMessage changedCard

                        updatedCards =
                            case event of
                                CardEditor.Save ->
                                    replaceCardInArray previousCard updatedCard model.cards

                                _ ->
                                    model.cards

                        cardBeingEdited : CardBeingEdited
                        cardBeingEdited =
                            case event of
                                CardEditor.NoEvent ->
                                    Card previousCard updatedCard

                                _ ->
                                    NoCard
                    in
                        ( { model | cardBeingEdited = cardBeingEdited, cards = updatedCards }, command )

        EditCard card ->
            ( { model | cardBeingEdited = Card card card }, Cmd.none )

        SaveDeck ->
            ( model, sendToSaveModule model )


updateCard : CardEditor.Msg -> CardEditor.Model -> ( CardEditor.Model, Cmd Msg, CardEditor.Event )
updateCard msg model =
    let
        ( updatedCard, command, event ) =
            CardEditor.update msg model
    in
        ( updatedCard, Cmd.map (CardMessage updatedCard) command, event )


replaceCardInArray : CardEditor.Model -> CardEditor.Model -> Array CardEditor.Model -> Array CardEditor.Model
replaceCardInArray oldCard newCard cards =
    findIndexForCard oldCard cards
        |> Maybe.map (\id -> Array.Extra.update id (always newCard) cards)
        |> Maybe.withDefault cards


findIndexForCard : CardEditor.Model -> Array CardEditor.Model -> Maybe Int
findIndexForCard cardToFind cards =
    Array.indexedMap (,) cards
        |> Array.foldl
            (\( id, card ) match ->
                case match of
                    Nothing ->
                        if card == cardToFind then
                            Just id
                        else
                            Nothing

                    _ ->
                        match
            )
            Nothing



-- VIEW


view : Model -> Html.Html Msg
view model =
    div []
        [ tachyons.css
        , viewCards model.cards
        , case model.cardBeingEdited of
            NoCard ->
                text ""

            Card _ cardModel ->
                viewCardBeingEdited cardModel
        ]


viewCards : Array CardEditor.Model -> Html.Html Msg
viewCards cards =
    cards
        |> Array.map (\card -> ( card, CardEditor.viewStaticCard "300px" "600px" card ))
        |> Array.toList
        |> List.map
            (\( card, cardHtml ) ->
                div
                    [ classes [ mr2 ]
                    , style [ ( "zoom", "0.5" ) ]
                    , onClick (EditCard card)
                    ]
                    [ cardHtml ]
            )
        |> div [ classes [ flex ] ]


viewCardBeingEdited : CardEditor.Model -> Html.Html Msg
viewCardBeingEdited card =
    div [ classes [ flex, mt2 ] ]
        [ viewCard card |> Html.map CardBeingEditedMessage
        , viewCardController card |> Html.map CardBeingEditedMessage
        ]



-- PORTS


sendToSaveModule : Model -> Cmd Msg
sendToSaveModule model =
    Json.Encode.encode 0 (encodeModel model)
        |> toSaveModule


port toSaveModule : String -> Cmd msg


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "cards", Array.Extra.filterMap encodeCard model.cards |> Json.Encode.array )
        , ( "autosave", encodeAutoSave model.saveOption )
        ]


encodeAutoSave : SaveOption -> Json.Encode.Value
encodeAutoSave saveOption =
    (case saveOption of
        AutoSave ->
            True

        ManualOnly ->
            False
    )
        |> Json.Encode.bool



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.saveOption of
            AutoSave ->
                Time.every (30 * Time.second) (always SaveDeck)

            ManualOnly ->
                Sub.none
        , case model.cardBeingEdited of
            NoCard ->
                Sub.none

            Card _ card ->
                CardEditor.subscriptions card |> Sub.map CardBeingEditedMessage
        ]
