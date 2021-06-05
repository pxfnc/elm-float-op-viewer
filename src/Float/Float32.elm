module Float.Float32 exposing
    ( Float32
    , fromFloat
    , toString
    )

{-| 32 bit float

@doc Float32
@doc fromFloat
@doc toString
@doc zero

-}

import Bytes exposing (Bytes)
import Bytes.Decode as D
import Bytes.Encode as E
import List.Extra


endian : Bytes.Endianness
endian =
    Bytes.BE


{-| 32 bit float
-}
type Float32
    = Float32 Bytes -- should not export `Float32` constructor


fromFloat : Float -> Float32
fromFloat value =
    Float32 (E.encode (E.float32 endian value))


intToBitString : Int -> String
intToBitString value =
    case value of
        0 ->
            "0"

        other ->
            other
                |> List.Extra.unfoldr
                    (\n ->
                        if n == 0 then
                            Nothing

                        else
                            Just ( String.fromInt (modBy 2 n), n // 2 )
                    )
                |> List.reverse
                |> String.concat


decodeFloat32BitStr : D.Decoder String
decodeFloat32BitStr =
    D.unsignedInt32 endian
        |> D.map intToBitString
        |> D.map (String.padLeft 32 '0')


{-| helper function. convert input string to [<sign> | <exponent> | <fraction>]
-}
format32BitStr : String -> String
format32BitStr input =
    input
        |> String.foldr
            (\c ( acc, index ) ->
                ( if index == 1 || index == 8 then
                    String.cons c (" | " ++ acc)

                  else
                    String.cons c acc
                , index - 1
                )
            )
            ( "", 32 )
        |> Tuple.first
        |> (\ipt -> "[" ++ ipt ++ "]")


{-| to human readable string

    toString (fromFloat 0)
    --> "[0 | 0000000 | 000000000000000000000000]"

    toString (fromFloat (2 ^ 127))
    --> "[0 | 1111111 | 000000000000000000000000]"

-}
toString : Float32 -> String
toString (Float32 value) =
    value
        |> D.decode (decodeFloat32BitStr |> D.map format32BitStr)
        |> Maybe.withDefault "<!Error>"
