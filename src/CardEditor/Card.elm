module CardEditor.Card
    exposing
        ( Model
        , CardId
        , Content(Content)
        , HiddenCard
        , DragStartDetails
        , DragEndDetails
        , Drag
        , decoder
        , encode
        , dragEndDecoder
        , contentToString
        , createCardCommand
        )

import Mouse exposing (Position)
import Array exposing (Array)
import Uuid
import Json.Decode.Pipeline as JsonPipeline
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random.Pcg


type alias Model =
    { nextHiddenCardId : Int
    , cardContent : Content
    , hiddenCards : Array HiddenCard
    , draggedHiddenCard : Maybe (Drag HiddenCard)
    , id : CardId
    }


type CardId
    = CardId Uuid.Uuid


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
    JsonPipeline.decode Model
        |> JsonPipeline.required "nextHiddenCardId" Decode.int
        |> JsonPipeline.required "cardContent" cardContentDecoder
        |> JsonPipeline.required "hiddenCards" (Decode.array hiddenCardDecoder)
        |> JsonPipeline.hardcoded Nothing
        |> JsonPipeline.required "id" cardIdDecoder


cardContentDecoder : Decoder Content
cardContentDecoder =
    Decode.string |> Decode.map Content


hiddenCardDecoder : Decoder HiddenCard
hiddenCardDecoder =
    JsonPipeline.decode HiddenCard
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
    Uuid.decoder |> Decode.map CardId


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "nextHiddenCardId", Encode.int model.nextHiddenCardId )
        , ( "cardContent", contentToString model.cardContent |> Encode.string )
        , ( "hiddenCards", Array.map encodeHiddenCard model.hiddenCards |> Encode.array )
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
    Uuid.toString cardId


createCardCommand : Content -> (Model -> msg) -> Cmd msg
createCardCommand content event =
    Random.Pcg.generate event
        (idGenerator
            |> Random.Pcg.map (Model 1 content Array.empty Nothing)
        )


idGenerator : Random.Pcg.Generator CardId
idGenerator =
    Uuid.uuidGenerator |> Random.Pcg.map CardId


contentToString : Content -> String
contentToString (Content content) =
    content


dragEndDecoder : Decoder DragEndDetails
dragEndDecoder =
    Decode.map2 DragEndDetails
        (Decode.map2 Position
            (Decode.at [ "finalPosition", "x" ] Decode.int)
            (Decode.at [ "finalPosition", "y" ] Decode.int)
        )
        (Decode.map2 Position
            (Decode.at [ "parentPosition", "x" ] Decode.int)
            (Decode.at [ "parentPosition", "y" ] Decode.int)
        )
