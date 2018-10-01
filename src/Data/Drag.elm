module Data.Drag exposing (Drag, DragEndDetails, DragStartDetails, dragEndDecoder)

import Data.Position exposing (Position)
import Json.Decode as Decode exposing (Decoder)


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
