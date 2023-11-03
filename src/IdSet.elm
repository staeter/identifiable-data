module IdSet exposing (..)

import Id exposing (Id(..))
import Set exposing (Set)


type IdSet t
    = IdSet
        { content : Set Int
        , highestId : Int
        }


empty : IdSet t
empty =
    { content = Set.empty, highestId = -1 }
        |> IdSet


singleton : Id t -> IdSet t
singleton (Id id) =
    { content = Set.singleton id
    , highestId = id
    }
        |> IdSet


toList : IdSet t -> List (Id t)
toList (IdSet set) =
    Set.toList set.content
        |> List.map Id


uniqueId : IdSet t -> Id t
uniqueId (IdSet set) =
    Id (set.highestId + 1)


insert : Id t -> IdSet t -> IdSet t
insert (Id id) (IdSet set) =
    { content = Set.insert id set.content
    , highestId = max id set.highestId
    }
        |> IdSet


add : IdSet t -> ( Id t, IdSet t )
add (IdSet set) =
    let
        newId =
            set.highestId + 1
    in
    ( Id newId
    , { set
        | content = Set.insert newId set.content
        , highestId = newId
      }
        |> IdSet
    )


fromList : List (Id t) -> IdSet t
fromList list =
    List.map (\(Id id) -> id) list
        |> Set.fromList
        |> \set ->
            { content = set
            , highestId = Set.foldl max -1 set
            }
                |> IdSet


member : Id t -> IdSet t -> Bool
member (Id id) (IdSet set) =
    Set.member id set.content
