module IdDict exposing (..)

import Dict exposing (Dict)
import Id exposing (Id(..))
import IdSet exposing (IdSet(..))


type IdDict t value
    = IdDict
        { content : Dict Int value
        , highestId : Int
        }


type alias IdTable value =
    IdDict value value


empty : IdDict t value
empty =
    { content = Dict.empty
    , highestId = -1
    }
        |> IdDict


singleton : Id t -> value -> IdDict t value
singleton (Id id) val =
    { content = Dict.singleton id val
    , highestId = id
    }
        |> IdDict


get : Id t -> IdDict t value -> Maybe value
get (Id id) (IdDict table) =
    Dict.get id table.content


uniqueId : IdDict t value -> Id t
uniqueId (IdDict table) =
    Id (table.highestId + 1)


insert : Id t -> value -> IdDict t value -> IdDict t value
insert (Id id) val (IdDict table) =
    { content = Dict.insert id val table.content
    , highestId = max id table.highestId
    }
        |> IdDict


insertList : List ( Id t, value ) -> IdDict t value -> IdDict t value
insertList valList table =
    List.foldl
        (\( id, val ) table_ ->
            insert id val table_
        )
        table
        valList


add : value -> IdDict t value -> ( Id t, IdDict t value )
add val table =
    let
        addedId =
            uniqueId table
    in
    insert addedId val table
        |> Tuple.pair addedId


addList : List value -> IdDict t value -> ( List (Id t), IdDict t value )
addList valList table =
    List.foldl
        (\val ( idList, table_ ) ->
            let
                ( newId, newIdTable ) =
                    add val table_
            in
            ( newId :: idList, newIdTable )
        )
        ( [], table )
        valList
        |> Tuple.mapFirst List.reverse


toList : IdDict t value -> List ( Id t, value )
toList (IdDict table) =
    Dict.toList table.content
        |> List.map (Tuple.mapFirst Id)


fromList : List ( Id t, value ) -> IdDict t value
fromList valList =
    insertList valList empty


keys : IdDict t value -> List (Id t)
keys (IdDict table) =
    Dict.keys table.content
        |> List.map Id


update : Id t -> (Maybe value -> Maybe value) -> IdDict t value -> IdDict t value
update (Id id) func (IdDict table) =
    { table | content = Dict.update id func table.content }
        |> IdDict


remove : Id t -> IdDict t value -> IdDict t value
remove (Id id) (IdDict table) =
    { table | content = Dict.remove id table.content }
        |> IdDict


map : (Id t -> a -> b) -> IdDict t a -> IdDict t b
map func (IdDict table) =
    { content =
        Dict.map
            (\key val ->
                func (Id key) val
            )
            table.content
    , highestId = table.highestId
    }
        |> IdDict


union : IdDict t value -> IdDict t value -> IdDict t value
union (IdDict table1) (IdDict table2) =
    { content = Dict.union table1.content table2.content
    , highestId = max table1.highestId table2.highestId
    }
        |> IdDict


highestId : IdDict t value -> Maybe (Id t, value)
highestId (IdDict table) =
    Dict.foldl
        (\id val maybeMax ->
            case maybeMax of
                Nothing ->
                    Just (id, val)

                Just (maxId, _) ->
                    if id > maxId then
                        Just (id, val)
                    else
                        maybeMax
        )
        Nothing
        table.content
        |> Maybe.map (Tuple.mapFirst Id)


lowestId : IdDict t value -> Maybe (Id t, value)
lowestId (IdDict table) =
    Dict.foldl
        (\id val maybeMin ->
            case maybeMin of
                Nothing ->
                    Just (id, val)

                Just (minId, _) ->
                    if id < minId then
                        Just (id, val)
                    else
                        maybeMin
        )
        Nothing
        table.content
        |> Maybe.map (Tuple.mapFirst Id)


isEmpty : IdDict t value -> Bool
isEmpty (IdDict table) =
    Dict.isEmpty table.content


filter : (Id t -> value -> Bool) -> IdDict t value -> IdDict t value
filter func (IdDict dict) =
    { dict | content = Dict.filter (\id val -> func (Id id) val) dict.content }
        |> IdDict
