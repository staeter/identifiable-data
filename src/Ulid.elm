module Ulid exposing
    ( Ulid(..)
    , generator, stringGenerator
    , toString, fromString, toTimestamp
    , codec
    , toUuidString, fromUuidString
    )

{-| A library for generating ULID and utility functions. This module is an fork from [kyasu1/elm-ulid](https://github.com/kyasu1/elm-ulid/blob/1.0.0/src/Ulid.elm) but I modified it to match my needs.


# Definition

@docs Ulid


# Random Generators

@docs ulidGenerator, stringGenerator


# Conversion functions

@docs toString, fromString, toTimestamp


# Codec

@docs codec


# Utility functions for UUID string

@docs toUuidString, fromUuidString

-}

import Serialize
import Time.NoOpaqueType as Time exposing (Posix)
import Array exposing (Array)
import Random.NoOpaqueType as Random exposing (Generator, int, list, map, map2)
import Regex

{-| type for ULID.

ULID is composed of a Timestamp and a Randomness part with the following format.

     01AN4Z07BY      79KA1307SR9X4MV3

    |----------|    |----------------|
     Timestamp          Randomness
       48bits             80bits

The t type is there to indicate what data the Ulid is pointing to.
-}
type Ulid t
    = Ulid String


{-| Random generator for a single encapsulated ULID at specified time.
-}
generator : Posix -> Generator (Ulid t)
generator posix =
    map Ulid (stringGenerator posix)


{-| Random generator for a single ULID String at specified time.
-}
stringGenerator : Posix -> Generator String
stringGenerator posix =
    map2 (++) (encodeTime posix |> Random.constant) randomGenerator


{-| Extract the string representation.
-}
toString : Ulid t -> String
toString (Ulid ulid) =
    ulid


{-| Try to parse a string representation as an ULID, ie. 26 character length with valid characters of
`0123456789ABCDEFGHJKMNPQRSTVWXYZ`. Detect overflowing if the ULID is larger than `7ZZZZZZZZZZZZZZZZZZZZZZZZZ`.
-}
fromString : String -> Maybe (Ulid t)
fromString s =
    if isValid s then
        Just (Ulid s)

    else
        Nothing


{-| Extract the Timestamp part only.
-}
toTimestamp : Ulid t -> Posix
toTimestamp (Ulid ulid) =
    decodeTime ulid


{-| [MartinSStewart/elm-serialize](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/1.3.0/)'s codec serve as an encoder-decoder pair.
It guarantees that your decoder return the input that was given to the encoder.
-}
codec : Serialise.Codec (Ulid t)
codec =
    Serialize.tuple Serialize.string Serialize.int
        |> Serialize.mapValid
            (\( t, id ) ->
                if t == typeStr then
                    Ok (Ulid id)

                else
                    Err "Wrong Ulid type"
            )
            (\( Ulid id ) ->
                ( typeStr, id )
            )



{-| Produce a canonical 8-4-4-4-12 UUID string form.
The variant and version indicators of converted UUID are meaningless,
so the `Uuid.fromString` in [dandy32/elm-uuid] will fail.

[dandy32/elm-uuid]: https://package.elm-lang.org/packages/danyx23/elm-uuid/latest/

-}
toUuidString : Ulid t -> String
toUuidString (Ulid ulid) =
    ulid
        |> String.toList
        |> List.map base32ToBits
        |> List.concat
        |> bitsToBaseN 4
        |> List.drop 1
        |> toUuidStringInternal
        |> String.toLower

toUuidStringInternal : List Int -> String
toUuidStringInternal thirtyTwoHexDigits =
    let
        chars =
            thirtyTwoHexDigits |> List.map base32ToChar
    in
    String.fromList <|
        List.concat
            [ chars |> List.take 8
            , [ '-' ]
            , chars |> List.drop 8 |> List.take 4
            , [ '-' ]
            , chars |> List.drop 12 |> List.take 4
            , [ '-' ]
            , chars |> List.drop 16 |> List.take 4
            , [ '-' ]
            , chars |> List.drop 20 |> List.take 12
            ]

{-| Try to parse an UUID string, though the timestamp is meaningless.
If it overflows `Nothing` value will be returned.
-}
fromUuidString : String -> Maybe (Ulid t)
fromUuidString uuid =
    uuid
        |> String.replace "-" ""
        |> String.toUpper
        |> String.toList
        |> List.map hexToBits
        |> List.concat
        |> bitsToBaseN 5
        |> List.map base32ToChar
        |> String.fromList
        |> (\ulid ->
                if isValid ulid then
                    Just ulid

                else
                    Nothing
           )
        |> Maybe.map Ulid




-- Internal


encoding : Array Char
encoding =
    String.toList "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
        |> Array.fromList


base32ToChar : Int -> Char
base32ToChar i =
    Array.get i encoding
        |> Maybe.withDefault 'x'


ulidRegex : Regex.Regex
ulidRegex =
    Regex.fromStringWith { caseInsensitive = True, multiline = False } "[0-9A-FGHJKMNP-TV-Z]"
        |> Maybe.withDefault Regex.never


encodingLength : Int
encodingLength =
    Array.length encoding


isValid : String -> Bool
isValid s =
    if String.length s == 26 && Regex.contains ulidRegex s then
        -- check that the left most char has the code less than 56 which is '8' to detect overflowing
        case s |> String.toList |> List.head |> Maybe.map Char.toCode of
            Just code ->
                if code < 56 then
                    True

                else
                    False

            Nothing ->
                False

    else
        False


encodeTime : Posix -> String
encodeTime posix =
    Time.posixToMillis posix
        |> encodeTimeHelper 10
        |> List.reverse
        |> String.fromList


encodeTimeHelper : Int -> Int -> List Char
encodeTimeHelper digits time =
    if digits == 0 then
        []

    else
        let
            mod =
                modBy encodingLength time
        in
        base32ToChar mod :: encodeTimeHelper (digits - 1) (floor <| toFloat (time - mod) / toFloat encodingLength)


decodeTime : String -> Posix
decodeTime s =
    let
        bits =
            String.toList s
                |> List.take 10
                |> List.map base32ToBits
                |> List.concat

        len =
            List.length bits
    in
    bitsToInt len bits
        |> Time.millisToPosix


randomGenerator : Generator String
randomGenerator =
    list 16 base32Generator
        |> map (\list -> List.map base32ToChar list)
        |> map String.fromList


base32Generator : Generator Int
base32Generator =
    int 0 (encodingLength - 1)






-- Bit operations


bitsToBaseN : Int -> List Int -> List Int
bitsToBaseN baseN bits =
    let
        res =
            modBy baseN (List.length bits)

        padded =
            if res == 0 then
                bits

            else
                List.repeat (baseN - res) 0 ++ bits
    in
    bitsToBaseNHelper baseN padded


bitsToBaseNHelper : Int -> List Int -> List Int
bitsToBaseNHelper baseN bits =
    case bits of
        [] ->
            []

        _ ->
            bitsToInt baseN (List.take baseN bits) :: bitsToBaseNHelper baseN (List.drop baseN bits)


bitsToInt : Int -> List Int -> Int
bitsToInt baseN bits =
    case bits of
        [] ->
            0

        x :: xs ->
            x * (2 ^ (baseN - 1)) + bitsToInt (baseN - 1) xs


hexToBits : Char -> List Int
hexToBits c =
    case c of
        '0' ->
            [ 0, 0, 0, 0 ]

        '1' ->
            [ 0, 0, 0, 1 ]

        '2' ->
            [ 0, 0, 1, 0 ]

        '3' ->
            [ 0, 0, 1, 1 ]

        '4' ->
            [ 0, 1, 0, 0 ]

        '5' ->
            [ 0, 1, 0, 1 ]

        '6' ->
            [ 0, 1, 1, 0 ]

        '7' ->
            [ 0, 1, 1, 1 ]

        '8' ->
            [ 1, 0, 0, 0 ]

        '9' ->
            [ 1, 0, 0, 1 ]

        'A' ->
            [ 1, 0, 1, 0 ]

        'B' ->
            [ 1, 0, 1, 1 ]

        'C' ->
            [ 1, 1, 0, 0 ]

        'D' ->
            [ 1, 1, 0, 1 ]

        'E' ->
            [ 1, 1, 1, 0 ]

        'F' ->
            [ 1, 1, 1, 1 ]

        _ ->
            []


base32ToBits : Char -> List Int
base32ToBits c =
    if Char.isDigit c || (Char.toCode c >= 65 && Char.toCode c <= 70) then
        0 :: hexToBits c

    else
        case c of
            'G' ->
                -- 16
                [ 1, 0, 0, 0, 0 ]

            'H' ->
                -- 17
                [ 1, 0, 0, 0, 1 ]

            'J' ->
                -- 18
                [ 1, 0, 0, 1, 0 ]

            'K' ->
                -- 19
                [ 1, 0, 0, 1, 1 ]

            'M' ->
                -- 20
                [ 1, 0, 1, 0, 0 ]

            'N' ->
                -- 21
                [ 1, 0, 1, 0, 1 ]

            'P' ->
                -- 22
                [ 1, 0, 1, 1, 0 ]

            'Q' ->
                -- 23
                [ 1, 0, 1, 1, 1 ]

            'R' ->
                -- 24
                [ 1, 1, 0, 0, 0 ]

            'S' ->
                -- 25
                [ 1, 1, 0, 0, 1 ]

            'T' ->
                -- 26
                [ 1, 1, 0, 1, 0 ]

            'V' ->
                -- 27
                [ 1, 1, 0, 1, 1 ]

            'W' ->
                -- 28
                [ 1, 1, 1, 0, 0 ]

            'X' ->
                -- 29
                [ 1, 1, 1, 0, 1 ]

            'Y' ->
                -- 30
                [ 1, 1, 1, 1, 0 ]

            'Z' ->
                -- 31
                [ 1, 1, 1, 1, 1 ]

            _ ->
                []
