port module Main exposing (Model, Msg(..), Page(..), SaveOption(..), autoSaveDecoder, cardsArrayDecoder, deckPageConfig, encodeAutoSave, encodeModel, init, main, modelDecoder, newGameSet, replaceCardInArray, sendToSaveModule, setRoute, subscriptions, toSaveModule, update, view)

import Array exposing (Array)
import Browser
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra
import Data.Card as Card exposing (Content(..))
import Html exposing (div, h1, text)
import Json.Decode as Decode exposing (Value)
import Json.Decode.Pipeline as JsonPipeline
import Json.Encode
import Pages.CardEditor as CardEditorPage
import Pages.Deck
import Pages.PrintDeck as PrintDeck
import Route
import Tachyons.Tachyons exposing (tachyons)
import Time
import Url exposing (Url)


type Page
    = DeckPage String
    | CardEditorPage Card.Model CardEditorPage.Model
    | PrintDeckPage String


main : Program String Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = Route.fromUrl >> SetRoute
        }


init : String -> Url -> Key -> ( Model, Cmd Msg )
init savedGame url key =
    let
        decodedModelResult =
            Decode.decodeString (modelDecoder key) savedGame

        ( model, initialCommand ) =
            case decodedModelResult of
                Ok decodedModel ->
                    ( decodedModel, Cmd.none )

                Err _ ->
                    newGameSet key
    in
    case Route.fromUrl url of
        Just (Route.DeckCard _ cardId) ->
            Array.filter (\card -> (Card.toCardModel card |> .id) == cardId) model.cards
                |> Array.get 0
                |> Maybe.map (\card -> CardEditorPage card (CardEditorPage.init card))
                |> Maybe.map (\cardPage -> { model | page = cardPage })
                |> Maybe.withDefault model
                |> Cmd.Extra.withNoCmd

        Just (Route.PrintDeck deckId) ->
            ( { model | page = PrintDeckPage deckId }, Cmd.none )

        _ ->
            ( model, Cmd.none )


deckPageConfig : Pages.Deck.Config Msg
deckPageConfig =
    { createMsg = CreateCard
    , exportDeck = ExportDeck
    , importDeck = ImportDeck
    , onEditCard = EditCard
    , onRemoveCard = RemoveCard
    }


newGameSet : Key -> ( Model, Cmd Msg )
newGameSet key =
    let
        cardCommand =
            Card.createIllustrationAndTextCardCommand 1 (Card.stringToContent "Random content") CardCreated
    in
    ( Model Array.empty (DeckPage "randomId") ManualOnly key, cardCommand )



-- MODEL


type alias Model =
    { cards : Array Card.Model
    , page : Page
    , saveOption : SaveOption
    , navigationKey : Key
    }


type SaveOption
    = AutoSave
    | ManualOnly


modelDecoder : Key -> Decode.Decoder Model
modelDecoder key =
    Decode.succeed Model
        |> JsonPipeline.required "cards" cardsArrayDecoder
        |> JsonPipeline.hardcoded (DeckPage "randomId")
        |> JsonPipeline.required "autosave" autoSaveDecoder
        |> JsonPipeline.hardcoded key


cardsArrayDecoder : Decode.Decoder (Array Card.Model)
cardsArrayDecoder =
    Decode.array Card.decoder


autoSaveDecoder : Decode.Decoder SaveOption
autoSaveDecoder =
    Decode.bool
        |> Decode.map
            (\isAutoSave ->
                if isAutoSave then
                    AutoSave

                else
                    ManualOnly
            )



-- UPDATE


type Msg
    = NoOp
    | CardEditorPageMessage CardEditorPage.Msg
    | EditCard Card.Model
    | SaveDeck
    | ImportDeck
    | ExportDeck
    | DeckImported (Array Card.Model)
    | CreateCard Card.CardType
    | RemoveCard Card.Model
    | CardCreated Card.Model
    | SetRoute (Maybe Route.Route)
    | UrlRequest Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, Debug.log "msg" msg ) of
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

                ( newPage, newPageCommand ) =
                    case event of
                        CardEditorPage.NoEvent ->
                            ( CardEditorPage cardModel updatedModel, Cmd.none )

                        _ ->
                            ( model.page, Route.newUrl model.navigationKey Route.Home )
            in
            ( { model | page = newPage, cards = updatedCards }
            , Cmd.batch
                [ Cmd.map CardEditorPageMessage command
                , newPageCommand
                ]
            )

        ( DeckPage _, EditCard card ) ->
            let
                newUrlCmd =
                    Route.DeckCard "1" (Card.toCardModel card |> .id)
                        |> Route.newUrl model.navigationKey
            in
            ( { model | page = CardEditorPage card (CardEditorPage.init card) }, newUrlCmd )

        ( _, SaveDeck ) ->
            ( model, sendToSaveModule model )

        ( _, ExportDeck ) ->
            ( model, sendToExportModule model.cards )

        ( _, ImportDeck ) ->
            ( model, importDeck "deckImportInput" )

        ( _, DeckImported importedCards ) ->
            ( { model | cards = Array.append model.cards importedCards }, Cmd.none )

        ( DeckPage _, CreateCard cardType ) ->
            let
                biggestCardNumber =
                    Array.toList model.cards
                        |> List.map Card.toCardModel
                        |> List.map .number
                        |> List.maximum
                        |> Maybe.withDefault 1

                newCardCommand =
                    case cardType of
                        Card.FullIllustration ->
                            Card.createFullIllustrationCardCommand (biggestCardNumber + 1) CardCreated

                        Card.IllustrationAndText ->
                            Card.createIllustrationAndTextCardCommand (biggestCardNumber + 1) (Card.stringToContent "The content of the card goes here") CardCreated
            in
            ( model, newCardCommand )

        ( DeckPage _, RemoveCard cardToRemove ) ->
            let
                newCards =
                    Array.filter ((/=) cardToRemove) model.cards
            in
            ( { model | cards = newCards }, Cmd.none )

        ( DeckPage _, CardCreated cardModel ) ->
            ( { model | cards = Array.push cardModel model.cards }, Cmd.none )

        ( _, SetRoute routeMaybe ) ->
            setRoute routeMaybe model

        ( _, UrlRequest urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Route.fromUrl url
                                |> Maybe.map (Route.newUrl model.navigationKey)
                                |> Maybe.withDefault Cmd.none
                            )

                Browser.External href ->
                    ( model, Route.load href )

        _ ->
            ( model, Cmd.none )


setRoute : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
setRoute routeMaybe model =
    case routeMaybe of
        Just route ->
            case route of
                Route.Home ->
                    ( { model | page = DeckPage "randomId" }, Cmd.none )

                Route.Deck deckId ->
                    ( { model | page = DeckPage deckId }, Cmd.none )

                Route.PrintDeck deckId ->
                    ( { model | page = PrintDeckPage deckId }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


replaceCardInArray : Card.Model -> Card.Model -> Array Card.Model -> Array Card.Model
replaceCardInArray oldCard newCard cards =
    Array.map
        (\card ->
            if card == oldCard then
                newCard

            else
                card
        )
        cards



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        pageTitle =
            "Escape Game Generator"
    in
    div []
        [ tachyons.css
        , h1 [] [ text pageTitle ]
        , case model.page of
            DeckPage deckId ->
                Pages.Deck.view deckPageConfig deckId model.cards

            CardEditorPage _ cardEditorPageModel ->
                CardEditorPage.view cardEditorPageModel |> Html.map CardEditorPageMessage

            PrintDeckPage _ ->
                PrintDeck.view model.cards
        ]
        |> List.singleton
        |> Browser.Document pageTitle



-- PORTS


sendToSaveModule : Model -> Cmd Msg
sendToSaveModule model =
    Json.Encode.encode 0 (encodeModel model)
        |> toSaveModule


port toSaveModule : String -> Cmd msg


sendToExportModule : Array Card.Model -> Cmd Msg
sendToExportModule cards =
    Json.Encode.object
        [ ( "version", Json.Encode.int 1 )
        , ( "cards", Json.Encode.array Card.encode cards )
        ]
        |> Json.Encode.encode 0
        |> toExportModule


port toExportModule : String -> Cmd msg


port importDeck : String -> Cmd msg


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "cards", Json.Encode.array Card.encode model.cards )
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


port deckImported : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.saveOption of
            AutoSave ->
                Time.every (30 * 1000) (always SaveDeck)

            ManualOnly ->
                Sub.none
        , case model.page of
            CardEditorPage _ card ->
                CardEditorPage.subscriptions card |> Sub.map CardEditorPageMessage

            _ ->
                Sub.none
        , deckImported (Decode.decodeString deckImportDecoder >> Result.withDefault NoOp)
        ]


deckImportDecoder : Decode.Decoder Msg
deckImportDecoder =
    Decode.field "cards" cardsArrayDecoder
        |> Decode.map DeckImported
