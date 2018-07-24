module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import CardEditor.CardEditor as CardEditor exposing (Content(Content), Event(Discard), createCard, viewCard, viewCardController)
import Html exposing (div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes exposing (flex, flex_row, mr0, mr1, mr2, mt2, z_0)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    let
        ( cardModel, cardCommand ) =
            createCard (Content "Random content")

        ( cardModel2, cardCommand2 ) =
            createCard (Content "Random content")
    in
        ( (Array.fromList [ cardModel, cardModel, cardModel, cardModel ] |> Model) NoCard
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
    , cardBeingEdited : CardBeingEdited
    }


type CardBeingEdited
    = NoCard
    | Card CardEditor.Model CardEditor.Model



-- UPDATE


type Msg
    = CardMessage CardEditor.Model CardEditor.Msg
    | CardBeingEditedMessage CardEditor.Msg
    | EditCard CardEditor.Model


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.cardBeingEdited of
        NoCard ->
            Sub.none

        Card _ card ->
            CardEditor.subscriptions card |> Sub.map CardBeingEditedMessage
