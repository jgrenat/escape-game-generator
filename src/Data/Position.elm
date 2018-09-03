module Data.Position exposing (Position, decoder)

import Json.Decode as Decode


type alias Position =
    { x : Int
    , y : Int
    }


decoder : Decode.Decoder Position
decoder =
    Decode.map2 Position
        (Decode.field "clientX" (Decode.float |> Decode.map round))
        (Decode.field "clientY" (Decode.float |> Decode.map round))
