module UlidDict exposing (..)

import Dict exposing (Dict)
import Ulid exposing (Ulid(..))

type UlidDict id v
    = UlidDict (Dict String v)

empty : UlidDict id v
empty =
    UlidDict Dict.empty


insert : Ulid id -> v -> UlidDict id v -> UlidDict id v
insert (Ulid id) value (UlidDict dict) =
    UlidDict (Dict.insert id value dict)

foldl : (Ulid id -> v -> a -> a) -> a -> UlidDict id v -> a
foldl f acc (UlidDict dict) =
    Dict.foldl (\key value acc_ -> f (Ulid key) value acc_) acc dict

update : Ulid id -> (Maybe v -> Maybe v) -> UlidDict id v -> UlidDict id v
update (Ulid ulid) f (UlidDict dict) =
    UlidDict (Dict.update ulid f dict)
