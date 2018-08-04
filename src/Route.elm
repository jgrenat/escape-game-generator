module Route exposing (Route(..), route, href, newUrl, modifyUrl, fromLocation)

import CardEditor.Card as Card
import Html.Attributes as Attributes
import Html exposing (Attribute)
import Navigation
import UrlParser as Url exposing ((</>), Parser, s)


type Route
    = Home
    | Deck String
    | DeckCard String Card.CardId


route : Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home (s "")
        , Url.map DeckCard (s "deck" </> Url.string </> s "cards" </> Card.idParser)
        , Url.map Deck (s "deck" </> Url.string)
        ]


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                DeckCard deckId cardId ->
                    [ "deck", deckId, "cards", Card.idToString cardId ]

                Deck deckId ->
                    [ "deck", deckId ]
    in
        "#/" ++ String.join "/" pieces


href : Route -> Attribute msg
href route =
    Attributes.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


newUrl : Route -> Cmd msg
newUrl =
    routeToString >> Navigation.newUrl


fromLocation : Navigation.Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        Url.parseHash route location
