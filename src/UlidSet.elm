module UlidSet exposing (..)

import Set exposing (Set)
import Ulid exposing (Ulid(..))

type UlidSet v =
    UlidSet (Set String)


empty : UlidSet v
empty =
    UlidSet Set.empty

singleton : Ulid v -> UlidSet v
singleton (Ulid id) =
    UlidSet (Set.singleton id)

member : Ulid v -> UlidSet v -> Bool
member (Ulid id) (UlidSet set) =
    Set.member id set

foldl : (Ulid v -> a -> a) -> a -> UlidSet v -> a
foldl f acc (UlidSet set) =
    Set.foldl (Ulid >> f) acc set

insert : Ulid v -> UlidSet v -> UlidSet v
insert (Ulid id) (UlidSet set) =
    UlidSet (Set.insert id set)

union : UlidSet v -> UlidSet v -> UlidSet v
union (UlidSet set1) (UlidSet set2) =
    UlidSet (Set.union set1 set2)
