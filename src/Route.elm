module Route exposing (Route(..), fromUrl, href, load, newUrl, routeParser)

import Browser.Navigation as Navigation exposing (Key)
import Data.Card as Card
import Html exposing (Attribute)
import Html.Attributes as Attributes
import Url
import Url.Parser as UrlParser exposing ((</>), Parser, s)


type Route
    = Home
    | Deck String
    | PrintDeck String
    | DeckCard String Card.CardId


routeParser : Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home (s "/")
        , UrlParser.map DeckCard (s "deck" </> UrlParser.string </> s "cards" </> Card.idParser)
        , UrlParser.map PrintDeck (s "deck" </> UrlParser.string </> s "print")
        , UrlParser.map Deck (s "deck" </> UrlParser.string)
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

                PrintDeck deckId ->
                    [ "deck", deckId, "print" ]
    in
    "#/" ++ String.join "/" pieces


href : Route -> Attribute msg
href linkRoute =
    Attributes.href (routeToString linkRoute)


newUrl : Key -> Route -> Cmd msg
newUrl key routeToTransform =
    routeToString routeToTransform
        |> Navigation.pushUrl key


load : String -> Cmd msg
load url =
    Navigation.load url


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> UrlParser.parse routeParser
