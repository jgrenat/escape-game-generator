port module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import CardEditor.Card as Card exposing (Content(Content), createCardCommand, idToString)
import Cmd.Extra
import Html exposing (button, div, h1, text)
import Json.Decode exposing (Value)
import Json.Decode.Pipeline as JsonPipeline
import Json.Encode
import Navigation
import Pages.CardEditor as CardEditorPage
import Pages.Deck
import Route
import Tachyons exposing (classes, tachyons)
import Time


type Page
    = DeckPage
    | CardEditorPage Card.Model CardEditorPage.Model


main : Program String Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : String -> Navigation.Location -> ( Model, Cmd Msg )
init savedGame location =
    let
        decodedModelResult =
            Json.Decode.decodeString modelDecoder savedGame

        ( model, initialCommand ) =
            case decodedModelResult of
                Ok model ->
                    ( model, Cmd.none )

                Err _ ->
                    newGameSet
    in
        case Route.fromLocation location of
            Just (Route.DeckCard _ cardId) ->
                Array.filter (\card -> card.id == cardId) model.cards
                    |> Array.get 0
                    |> Maybe.map (\card -> CardEditorPage card (CardEditorPage.init card))
                    |> Maybe.map (\cardPage -> { model | page = cardPage })
                    |> Maybe.withDefault model
                    |> Cmd.Extra.withNoCmd

            _ ->
                ( model, Cmd.none )


newGameSet : ( Model, Cmd Msg )
newGameSet =
    let
        cardCommand =
            createCardCommand 15 (Content "Random content") CardCreated
    in
        ( Model Array.empty DeckPage AutoSave, cardCommand )



-- MODEL


type alias Model =
    { cards : Array Card.Model
    , page : Page
    , saveOption : SaveOption
    }


type SaveOption
    = AutoSave
    | ManualOnly


modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    JsonPipeline.decode Model
        |> JsonPipeline.required "cards" cardsArrayDecoder
        |> JsonPipeline.hardcoded DeckPage
        |> JsonPipeline.required "autosave" autoSaveDecoder


cardsArrayDecoder : Json.Decode.Decoder (Array Card.Model)
cardsArrayDecoder =
    Json.Decode.array Card.decoder


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
    = CardEditorPageMessage CardEditorPage.Msg
    | EditCard Card.Model
    | SaveDeck
    | CreateCard
    | CardCreated Card.Model
    | SetRoute (Maybe Route.Route)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( CardEditorPage cardModel cardEditorPageModel, CardEditorPageMessage cardMessage ) ->
            let
                ( updatedModel, command, event ) =
                    CardEditorPage.update cardMessage cardEditorPageModel

                updatedCards =
                    case event of
                        CardEditorPage.Save cardToSave ->
                            replaceCardInArray cardModel cardToSave model.cards

                        _ ->
                            model.cards

                newPage =
                    case event of
                        CardEditorPage.NoEvent ->
                            CardEditorPage cardModel updatedModel

                        _ ->
                            DeckPage
            in
                ( { model | page = newPage, cards = updatedCards }, Cmd.map CardEditorPageMessage command )

        ( DeckPage, EditCard card ) ->
            let
                newUrlCmd =
                    Route.DeckCard "1" card.id
                        |> Route.newUrl
            in
                ( { model | page = CardEditorPage card (CardEditorPage.init card) }, newUrlCmd )

        ( _, SaveDeck ) ->
            ( model, sendToSaveModule model )

        ( DeckPage, CreateCard ) ->
            let
                biggestCardNumber =
                    Array.toList model.cards
                        |> List.map .number
                        |> List.maximum
                        |> Maybe.withDefault 1

                newCardCommand =
                    createCardCommand (biggestCardNumber + 1) (Content "The content of the card goes here") CardCreated
            in
                ( model, newCardCommand )

        ( DeckPage, CardCreated cardModel ) ->
            ( { model | cards = Array.push cardModel model.cards }, Cmd.none )

        ( _, SetRoute routeMaybe ) ->
            setRoute routeMaybe model

        _ ->
            ( model, Cmd.none )


setRoute : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
setRoute routeMaybe model =
    case routeMaybe of
        Just route ->
            case route of
                Route.Home ->
                    ( { model | page = DeckPage }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


replaceCardInArray : Card.Model -> Card.Model -> Array Card.Model -> Array Card.Model
replaceCardInArray oldCard newCard cards =
    findIndexForCard oldCard cards
        |> Maybe.map (\id -> Array.Extra.update id (always newCard) cards)
        |> Maybe.withDefault cards


findIndexForCard : Card.Model -> Array Card.Model -> Maybe Int
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
        , h1 [] [ text "Escape Game Generator" ]
        , case model.page of
            DeckPage ->
                Pages.Deck.view CreateCard EditCard model.cards

            CardEditorPage _ cardEditorPageModel ->
                CardEditorPage.view cardEditorPageModel |> Html.map CardEditorPageMessage
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
        [ ( "cards", Array.map Card.encode model.cards |> Json.Encode.array )
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
        , case model.page of
            CardEditorPage _ card ->
                CardEditorPage.subscriptions card |> Sub.map CardEditorPageMessage

            _ ->
                Sub.none
        ]
