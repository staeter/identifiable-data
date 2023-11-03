module Uuid exposing  (generator, isValid)

{-| This module provides helpers to generate new Uuids using
Random.NoGhostType package. This module is a slightly modified version of [danyx23/elm-uuid/2.1.2/Uuid/Barebones.elm](https://github.com/danyx23/elm-uuid/blob/2.1.2/src/Uuid/Barebones.elm).

Uuids are Universally Unique IDentifiers. They are 128 bit ids that are
designed to be extremely unlikely to collide with other Uuids.

This library only supports generating Version 4 Uuid (those generated using
random numbers, as opposed to hashing. See
[Wikipedia on Uuids](https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_.28random.29)
for more details). Version 4 Uuids are constructed using 122 pseudo random bits.

Disclaimer: If you use this Library to generate Uuids, please be advised
that it does not use a cryptographically secure pseudo random number generator.
Depending on your use case the randomness provided may not be enough. The
period of the underlying random generator is high, so creating lot's of random
UUIDs on one client is fine, but please be aware that since the initial random
seed of the current Random implementation is limited to 32 bits, creating
UUIDs on many independent clients may lead to collisions more quickly than you
think (see <https://github.com/danyx23/elm-uuid/issues/10> for details)!

@docs generator, isValid

-}

import Array
import Bitwise
import Char
import List
import Random.NoOpaqueType as Random exposing (int, list, map)
import Regex
import String


{-| Random Generator for Uuid Strings. Using this Generator instead of the generate
function lets you use the full power of the Generator library to create lists of Uuids,
map them to other types etc.
-}
generator : Random.Generator String
generator =
    map toUuidString (list 31 hexGenerator)


{-| Verification function to check if the given string is a valid Uuid in the canonical
representation xxxxxxxx-xxxx-Axxx-Yxxx-xxxxxxxxxxxx where A is the version number between
[1-5] and Y is in the range [8-B]
-}
isValid : String -> Bool
isValid uuidAsString =
    Regex.contains uuidRegex uuidAsString



{- Create a valid V4 Uuid from a list of 31 hex values. The final
   Uuid has 32 hex characters with 4 seperators. One of the characters
   is fixed to 4 to indicate the version, and one is limited to the range
   [8-B] (indicated with Y in the sample string):
   xxxxxxxx-xxxx-4xxx-Yxxx-xxxxxxxxxxxx
-}


toUuidString : List Int -> String
toUuidString thirtyOneHexDigits =
    String.concat
        [ thirtyOneHexDigits |> List.take 8 |> List.map mapToHex |> String.fromList
        , "-"
        , thirtyOneHexDigits |> List.drop 8 |> List.take 4 |> List.map mapToHex |> String.fromList
        , "-"
        , "4"
        , thirtyOneHexDigits |> List.drop 12 |> List.take 3 |> List.map mapToHex |> String.fromList
        , "-"
        , thirtyOneHexDigits |> List.drop 15 |> List.take 1 |> List.map limitDigitRange8ToB |> List.map mapToHex |> String.fromList
        , thirtyOneHexDigits |> List.drop 16 |> List.take 3 |> List.map mapToHex |> String.fromList
        , "-"
        , thirtyOneHexDigits |> List.drop 19 |> List.take 12 |> List.map mapToHex |> String.fromList
        ]


limitDigitRange8ToB : Int -> Int
limitDigitRange8ToB digit =
    Bitwise.or (Bitwise.and digit 3) 8


uuidRegex : Regex.Regex
uuidRegex =
    Regex.fromString "^[0-9A-Fa-f]{8,8}-[0-9A-Fa-f]{4,4}-[1-5][0-9A-Fa-f]{3,3}-[8-9A-Ba-b][0-9A-Fa-f]{3,3}-[0-9A-Fa-f]{12,12}$"
        |> Maybe.withDefault Regex.never


hexDigits : Array.Array Char
hexDigits =
    let
        mapChars offset digit =
            Char.fromCode <| digit + offset
    in
    List.map (mapChars 48) (List.range 0 9)
        ++ List.map (mapChars 97) (List.range 0 5)
        |> Array.fromList



{- Map an integer in the range 0-15 to a hexadecimal character -}


mapToHex : Int -> Char
mapToHex index =
    let
        maybeResult =
            Array.get index hexDigits
    in
    case maybeResult of
        Nothing ->
            'x'

        Just result ->
            result


hexGenerator : Random.Generator Int
hexGenerator =
    int 0 15
