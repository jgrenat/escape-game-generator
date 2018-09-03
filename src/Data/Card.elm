module Data.Card exposing
    ( CardId
    , Content(..)
    , Drag
    , DragEndDetails
    , DragStartDetails
    , HiddenCard
    , Model
    , contentToString
    , createCardCommand
    , decoder
    , dragEndDecoder
    , encode
    , idParser
    , idToString
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


type alias Model =
    { nextHiddenCardId : Int
    , number : Int
    , cardContent : Content
    , hiddenCards : Array HiddenCard
    , draggedHiddenCard : Maybe (Drag HiddenCard)
    , id : CardId
    }


type CardId
    = CardId String


type Content
    = Content String


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


type alias DragStartDetails =
    { initialPosition : Position
    , parentWidth : Int
    , parentHeight : Int
    , layerX : Int
    , layerY : Int
    }


type alias DragEndDetails =
    { finalPosition : Position
    , parentPosition : Position
    }


type alias Drag a =
    { element : a
    , currentTopOffset : Int
    , currentLeftOffset : Int
    , initialPosition : Position
    , parentWidth : Int
    , parentHeight : Int
    , layerX : Int
    , layerY : Int
    }


decoder : Decoder Model
decoder =
    Decode.succeed Model
        |> JsonPipeline.required "nextHiddenCardId" Decode.int
        |> JsonPipeline.required "number" Decode.int
        |> JsonPipeline.required "cardContent" cardContentDecoder
        |> JsonPipeline.required "hiddenCards" (Decode.array hiddenCardDecoder)
        |> JsonPipeline.hardcoded Nothing
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
    Encode.object
        [ ( "nextHiddenCardId", Encode.int model.nextHiddenCardId )
        , ( "number", Encode.int model.number )
        , ( "cardContent", contentToString model.cardContent |> Encode.string )
        , ( "hiddenCards", Encode.array encodeHiddenCard model.hiddenCards )
        , ( "id", idToString model.id |> Encode.string )
        ]


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


idToString : CardId -> String
idToString (CardId cardId) =
    cardId


createCardCommand : Int -> Content -> (Model -> msg) -> Cmd msg
createCardCommand number content event =
    Random.generate event
        (idGenerator
            |> Random.map (Model 1 number content Array.empty Nothing)
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


dragEndDecoder : Decoder DragEndDetails
dragEndDecoder =
    Decode.map2 DragEndDetails
        (Decode.map2 Position
            (Decode.at [ "finalPosition", "x" ] (Decode.map round Decode.float))
            (Decode.at [ "finalPosition", "y" ] (Decode.map round Decode.float))
        )
        (Decode.map2 Position
            (Decode.at [ "parentPosition", "x" ] (Decode.map round Decode.float))
            (Decode.at [ "parentPosition", "y" ] (Decode.map round Decode.float))
        )


idParser : Url.Parser.Parser (CardId -> a) a
idParser =
    Url.Parser.string |> Url.Parser.map CardId
