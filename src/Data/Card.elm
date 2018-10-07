module Data.Card exposing
    ( CardId
    , CardIllustration(..)
    , CardType(..)
    , CardTypeDetails(..)
    , Content
    , HiddenCard
    , IllustrationAndTextCardModel
    , Model
    , contentToString
    , createFullIllustrationCardCommand
    , createIllustrationAndTextCardCommand
    , decoder
    , encode
    , idParser
    , idToString
    , stringToContent
    , toCardModel
    , toCardTypeDetails
    , toNumber
    , updateCardContent
    , updateCardModel
    )

import Array exposing (Array)
import Data.Position exposing (Position)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as JsonPipeline
import Json.Encode as Encode
import Random
import Random.Char
import Random.String
import Url.Parser


type Model
    = FullIllustrationCard CardModel
    | IllustrationAndTextCard CardModel IllustrationAndTextCardModel


type CardTypeDetails
    = FullIllustrationCardDetails
    | IllustrationAndTextCardDetails IllustrationAndTextCardModel


type alias CardModel =
    { nextHiddenCardId : Int
    , number : Int
    , illustration : CardIllustration
    , hiddenCards : Array HiddenCard
    , id : CardId
    }


type alias IllustrationAndTextCardModel =
    { cardContent : Content }


type CardId
    = CardId String


type Content
    = Content String


type CardIllustration
    = NoIllustration
    | Base64 String
    | Url String


type CardType
    = FullIllustration
    | IllustrationAndText


type alias HiddenCard =
    { id : Int
    , number : Int
    , color : String
    , sizeInEm : Float
    , top : Float
    , left : Float
    , opacity : Float
    , rotation : Float
    }


decoder : Decoder Model
decoder =
    Decode.field "type" cardTypeDecoder
        |> Decode.andThen
            (\cardType ->
                case cardType of
                    IllustrationAndText ->
                        Decode.succeed IllustrationAndTextCard
                            |> JsonPipeline.custom cardModelDecoder
                            |> JsonPipeline.custom illustrationAndTextCardModelDecoder

                    FullIllustration ->
                        Decode.succeed FullIllustrationCard
                            |> JsonPipeline.custom cardModelDecoder
            )


cardTypeDecoder : Decoder CardType
cardTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\cardTypeString ->
                case cardTypeString of
                    "fullIllustrationCard" ->
                        Decode.succeed FullIllustration

                    "illustrationAndTextCard" ->
                        Decode.succeed IllustrationAndText

                    _ ->
                        "Unknown card type value "
                            ++ cardTypeString
                            |> Decode.fail
            )


illustrationAndTextCardModelDecoder : Decoder IllustrationAndTextCardModel
illustrationAndTextCardModelDecoder =
    Decode.succeed IllustrationAndTextCardModel
        |> JsonPipeline.required "cardContent" cardContentDecoder


cardModelDecoder : Decoder CardModel
cardModelDecoder =
    Decode.succeed CardModel
        |> JsonPipeline.required "nextHiddenCardId" Decode.int
        |> JsonPipeline.required "number" Decode.int
        |> JsonPipeline.optional "illustration" illustrationDecoder NoIllustration
        |> JsonPipeline.required "hiddenCards" (Decode.array hiddenCardDecoder)
        |> JsonPipeline.required "id" cardIdDecoder


cardContentDecoder : Decoder Content
cardContentDecoder =
    Decode.string |> Decode.map Content


hiddenCardDecoder : Decoder HiddenCard
hiddenCardDecoder =
    Decode.succeed HiddenCard
        |> JsonPipeline.required "id" Decode.int
        |> JsonPipeline.required "number" Decode.int
        |> JsonPipeline.required "color" Decode.string
        |> JsonPipeline.required "sizeInEm" Decode.float
        |> JsonPipeline.required "top" Decode.float
        |> JsonPipeline.required "left" Decode.float
        |> JsonPipeline.required "opacity" Decode.float
        |> JsonPipeline.required "rotation" Decode.float


cardIdDecoder : Decoder CardId
cardIdDecoder =
    Decode.string |> Decode.map CardId


encode : Model -> Encode.Value
encode model =
    let
        cardModel =
            toCardModel model

        ( typeField, specificFields ) =
            case model of
                IllustrationAndTextCard _ illustrationAndTextCardModel ->
                    ( Encode.string "illustrationAndTextCard"
                    , [ ( "cardContent", contentToString illustrationAndTextCardModel.cardContent |> Encode.string ) ]
                    )

                FullIllustrationCard _ ->
                    ( Encode.string "fullIllustrationCard", [] )
    in
    Encode.object
        ([ ( "type", typeField )
         , ( "nextHiddenCardId", Encode.int cardModel.nextHiddenCardId )
         , ( "number", Encode.int cardModel.number )
         , ( "illustration", encodeIllustration cardModel.illustration )
         , ( "hiddenCards", Encode.array encodeHiddenCard cardModel.hiddenCards )
         , ( "id", idToString cardModel.id |> Encode.string )
         ]
            ++ specificFields
        )


toCardModel : Model -> CardModel
toCardModel model =
    case model of
        FullIllustrationCard cardModel ->
            cardModel

        IllustrationAndTextCard cardModel _ ->
            cardModel


updateCardModel : CardModel -> Model -> Model
updateCardModel newCardModel model =
    case model of
        FullIllustrationCard _ ->
            FullIllustrationCard newCardModel

        IllustrationAndTextCard _ illustrationAndTextCardModel ->
            IllustrationAndTextCard newCardModel illustrationAndTextCardModel


updateCardContent : Content -> Model -> Model
updateCardContent newContent model =
    case model of
        FullIllustrationCard cardModel ->
            FullIllustrationCard cardModel

        IllustrationAndTextCard cardModel illustrationAndTextCardModel ->
            IllustrationAndTextCard cardModel { illustrationAndTextCardModel | cardContent = newContent }


toCardTypeDetails : Model -> CardTypeDetails
toCardTypeDetails model =
    case model of
        FullIllustrationCard _ ->
            FullIllustrationCardDetails

        IllustrationAndTextCard _ illustrationAndTextCardModel ->
            IllustrationAndTextCardDetails illustrationAndTextCardModel


encodeHiddenCard : HiddenCard -> Decode.Value
encodeHiddenCard hiddenCard =
    Encode.object
        [ ( "id", Encode.int hiddenCard.id )
        , ( "number", Encode.int hiddenCard.number )
        , ( "color", Encode.string hiddenCard.color )
        , ( "sizeInEm", Encode.float hiddenCard.sizeInEm )
        , ( "top", Encode.float hiddenCard.top )
        , ( "left", Encode.float hiddenCard.left )
        , ( "opacity", Encode.float hiddenCard.opacity )
        , ( "rotation", Encode.float hiddenCard.rotation )
        ]


encodeIllustration : CardIllustration -> Decode.Value
encodeIllustration illustration =
    case illustration of
        NoIllustration ->
            Encode.null

        Base64 base64 ->
            Encode.object [ ( "type", Encode.string "base64" ), ( "value", Encode.string base64 ) ]

        Url url ->
            Encode.object [ ( "type", Encode.string "url" ), ( "value", Encode.string url ) ]


idToString : CardId -> String
idToString (CardId cardId) =
    cardId


toNumber : Model -> Int
toNumber model =
    toCardModel model |> .number


createIllustrationAndTextCardCommand : Int -> Content -> (Model -> msg) -> Cmd msg
createIllustrationAndTextCardCommand number content event =
    Random.generate event
        (idGenerator
            |> Random.map (CardModel 1 number NoIllustration Array.empty)
            |> Random.map
                (\cardModel ->
                    IllustrationAndTextCardModel content
                        |> IllustrationAndTextCard cardModel
                )
        )


createFullIllustrationCardCommand : Int -> (Model -> msg) -> Cmd msg
createFullIllustrationCardCommand number event =
    Random.generate event
        (idGenerator
            |> Random.map (CardModel 1 number NoIllustration Array.empty)
            |> Random.map FullIllustrationCard
        )


idGenerator : Random.Generator CardId
idGenerator =
    Random.String.string 36 randomCharGenerator |> Random.map CardId


randomCharGenerator : Random.Generator Char
randomCharGenerator =
    Random.weighted ( 0.01, Random.constant '-' )
        [ ( 0.495, randomNumber )
        , ( 0.495, randomLowercaseLetter )
        ]
        |> Random.andThen identity


randomNumber : Random.Generator Char
randomNumber =
    Random.Char.char 48 57


randomLowercaseLetter : Random.Generator Char
randomLowercaseLetter =
    Random.Char.char 97 122


contentToString : Content -> String
contentToString (Content content) =
    content


stringToContent : String -> Content
stringToContent stringContent =
    Content stringContent


illustrationDecoder : Decoder CardIllustration
illustrationDecoder =
    Decode.oneOf
        [ Decode.map2 Tuple.pair (Decode.field "type" Decode.string) (Decode.field "value" Decode.string)
            |> Decode.andThen
                (\( illustrationType, value ) ->
                    case illustrationType of
                        "base64" ->
                            Base64 value |> Decode.succeed

                        "path" ->
                            Url value |> Decode.succeed

                        _ ->
                            Decode.fail "Invalid illustration type"
                )
        , Decode.null NoIllustration
        ]


idParser : Url.Parser.Parser (CardId -> a) a
idParser =
    Url.Parser.string |> Url.Parser.map CardId
