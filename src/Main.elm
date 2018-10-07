port module Main exposing (Model, Msg(..), Page(..), SaveOption(..), autoSaveDecoder, cardsArrayDecoder, deckPageConfig, encodeAutoSave, encodeModel, findIndexForCard, init, main, modelDecoder, newGameSet, replaceCardInArray, sendToSaveModule, setRoute, subscriptions, toSaveModule, update, view)

import Array exposing (Array)
import Browser
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra
import Data.Card as Card exposing (Content(..), idToString)
import Html exposing (button, div, h1, text)
import Json.Decode as Decode exposing (Value)
import Json.Decode.Pipeline as JsonPipeline
import Json.Encode
import Pages.CardEditor as CardEditorPage
import Pages.Deck
import Route
import Tachyons.Tachyons exposing (classes, tachyons)
import Time
import Url exposing (Url)


type Page
    = DeckPage
    | CardEditorPage Card.Model CardEditorPage.Model


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

        _ ->
            ( model, Cmd.none )


deckPageConfig : Pages.Deck.Config Msg
deckPageConfig =
    { createMsg = CreateCard
    , onEditCard = EditCard
    , onRemoveCard = RemoveCard
    }


newGameSet : Key -> ( Model, Cmd Msg )
newGameSet key =
    let
        cardCommand =
            Card.createIllustrationAndTextCardCommand 1 (Card.stringToContent "Random content") CardCreated
    in
    ( Model Array.empty DeckPage AutoSave key, cardCommand )



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
        |> JsonPipeline.hardcoded DeckPage
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
                            ( DeckPage, Route.newUrl model.navigationKey Route.Home )
            in
            ( { model | page = newPage, cards = updatedCards }
            , Cmd.batch
                [ Cmd.map CardEditorPageMessage command
                , newPageCommand
                ]
            )

        ( DeckPage, EditCard card ) ->
            let
                newUrlCmd =
                    Route.DeckCard "1" (Card.toCardModel card |> .id)
                        |> Route.newUrl model.navigationKey
            in
            ( { model | page = CardEditorPage card (CardEditorPage.init card) }, newUrlCmd )

        ( _, SaveDeck ) ->
            ( model, sendToSaveModule model )

        ( DeckPage, CreateCard cardType ) ->
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

        ( DeckPage, RemoveCard cardToRemove ) ->
            let
                newCards =
                    Array.filter ((/=) cardToRemove) model.cards
            in
            ( { model | cards = newCards }, Cmd.none )

        ( DeckPage, CardCreated cardModel ) ->
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
                    ( { model | page = DeckPage }, Cmd.none )

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


findIndexForCard : Card.Model -> Array Card.Model -> Maybe Int
findIndexForCard cardToFind cards =
    Array.indexedMap (\a b -> ( a, b )) cards
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
            DeckPage ->
                Pages.Deck.view deckPageConfig model.cards

            CardEditorPage _ cardEditorPageModel ->
                CardEditorPage.view cardEditorPageModel |> Html.map CardEditorPageMessage
        ]
        |> List.singleton
        |> Browser.Document pageTitle



-- PORTS


sendToSaveModule : Model -> Cmd Msg
sendToSaveModule model =
    Json.Encode.encode 0 (encodeModel model)
        |> toSaveModule


port toSaveModule : String -> Cmd msg


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
        ]
