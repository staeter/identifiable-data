module Id exposing (..)

import Serialize


type Id t
    = Id Int



--//  ██████╗ ██████╗ ██████╗ ███████╗ ██████╗
--// ██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔════╝
--// ██║     ██║   ██║██║  ██║█████╗  ██║
--// ██║     ██║   ██║██║  ██║██╔══╝  ██║
--// ╚██████╗╚██████╔╝██████╔╝███████╗╚██████╗
--//  ╚═════╝ ╚═════╝ ╚═════╝ ╚══════╝ ╚═════╝

{-| The string is there to make sure its the right Id type
-}
codec : String -> Serialize.Codec String (Id t)
codec typeStr =
    Serialize.tuple Serialize.string Serialize.int
        |> Serialize.mapValid
            (\( t, id ) ->
                if t == typeStr then
                    Ok (Id id)

                else
                    Err "Wrong Id type"
            )
            (\( Id id ) ->
                ( typeStr, id )
            )
