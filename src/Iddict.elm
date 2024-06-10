module Iddict exposing
    ( Iddict
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    , encode, decoder
    )

{-| The id-dict is a data type that lets us store values in a dictionary using
unique identifiers. This can be used as a dictionary where the keys do not
matter.

The benefit of the id-dict is that it generates the keys FOR you. This way, you
do not need to generate identifiers yourself.


## Id-dict

@docs Iddict


## Build

@docs empty, singleton, insert, update, remove


## Query

@docs isEmpty, member, get, size


## Lists

@docs keys, values, toList, fromList

## Transform

@docs map, foldl, foldr, filter, partition

## Combine

@docs union, intersect, diff, merge


## JSON coders

@docs encode, decoder

-}

import FastDict as Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E


{-| The Iddict data type.
-}
type Iddict a
    = Iddict
        { cursor : Int
        , dict : Dict Int a
        }


{-| Decode an id-dict from a JSON value.
-}
decoder : D.Decoder a -> D.Decoder (Iddict a)
decoder coder =
    D.map2
        (\c d ->
            Iddict
                { cursor =
                    Dict.keys d
                        |> List.maximum
                        |> Maybe.map ((+) 1)
                        |> Maybe.withDefault 0
                        |> max (Dict.size d)
                        |> max c
                , dict = d
                }
        )
        (D.field "cursor" D.int)
        ( D.keyValuePairs coder
            |> D.map
                (List.filterMap
                    (\(key, val) ->
                        String.toInt key
                            |> Maybe.map (\k -> Tuple.pair k val)
                    )
                )
            |> D.map Dict.fromList
            |> D.field "dict"
        )

{-| Keep a key-value pair when its key does not appear in the second id-dict.
-}
diff : Iddict a -> Iddict a -> Iddict a
diff (Iddict old) (Iddict toRemove) =
    Iddict { old | dict = Dict.diff old.dict toRemove.dict }

{-| Create an empty id-dict.
-}
empty : Iddict a
empty =
    Iddict
        { cursor = 0
        , dict = Dict.empty
        }

{-| Convert an association list into an id-dict. 
-}
fromList : List (Int, a) -> Iddict a
fromList items =
    Iddict
        { cursor =
            items
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault 0
        , dict = Dict.fromList items
        }

{-| Encode an id-dict to a JSON value.
-}
encode : (a -> E.Value) -> Iddict a -> E.Value
encode coder (Iddict d) =
    E.object
        [ ( "cursor", E.int d.cursor )
        , ( "dict"
          , d.dict
                |> Dict.toCoreDict
                |> E.dict String.fromInt coder
          )
        ]

{-| Keep only the key-value pairs that pass the given test.
-}
filter : (Int -> a -> Bool) -> Iddict a -> Iddict a
filter f (Iddict d) =
    Iddict { d | dict = Dict.filter f d.dict }

{-| Fold over the key-value pairs in an id-dict from lowest key to highest key.
-}
foldl : (Int -> a -> b -> b) -> b -> Iddict a -> b
foldl f start (Iddict d) =
    Dict.foldl f start d.dict

{-| Fold over the key-value pairs in an id-dict from highest key to lowest key.
-}
foldr : (Int -> a -> b -> b) -> b -> Iddict a -> b
foldr f start (Iddict d) =
    Dict.foldr f start d.dict

{-| Get a value from the id-dict using its key.
-}
get : Int -> Iddict a -> Maybe a
get k (Iddict { dict }) =
    Dict.get k dict


{-| Insert a new value into the id-dict. Given that the id-dict generates its
key, the function returns both the updated id-dict as the newly generated key.

    x = empty |> insert "hello" -- ( 0, <Iddict with value "hello"> )

    case x of
        ( _, id-dict ) ->
            get 0 id-dict -- Just "hello"

-}
insert : a -> Iddict a -> ( Int, Iddict a )
insert v (Iddict d) =
    ( d.cursor
    , Iddict { cursor = d.cursor + 1, dict = Dict.insert d.cursor v d.dict }
    )

{-| Keep a key-value pair when its key appears in the second id-dict.
Preference is given to values in the first id-dict.
-}
intersect : Iddict a -> Iddict a -> Iddict a
intersect (Iddict d1) (Iddict d2) =
    Iddict
        { cursor = Basics.max d1.cursor d2.cursor
        , dict = Dict.intersect d1.dict d2.dict
        }

{-| Determine if an id-dict is empty.
-}
isEmpty : Iddict a -> Bool
isEmpty (Iddict d) =
    Dict.isEmpty d.dict


{-| Get all of the keys from the id-dict, sorted from lowest to highest.
-}
keys : Iddict a -> List Int
keys (Iddict { dict }) =
    Dict.keys dict

{-| Apply a function to all values in an id-dict.
-}
map : (Int -> a -> b) -> Iddict a -> Iddict b
map f (Iddict d) =
    Iddict { cursor = d.cursor, dict = Dict.map f d.dict }

{-| Determine if a key is in an id-dict.
-}
member : Int -> Iddict a -> Bool
member k (Iddict d) =
    k < d.cursor && Dict.member k d.dict

{-| The most general way of combining two id-dicts. You provide three
accumulators for when a given key appears:


    1. Only in the left id-dict.
    2. In both id-dicts.
    3. Only in the right id-dict.

You then traverse all the keys from lowest to highest, building up whatever you
want.
-}
merge : (Int -> a -> result -> result)
    -> (Int -> a -> b -> result -> result)
    -> (Int -> b -> result -> result)
    -> Iddict a -> Iddict b -> result -> result
merge fA fBoth fB (Iddict a) (Iddict b) start =
    Dict.merge fA fBoth fB a.dict b.dict start

{-| Partition an id-dict according to some test. The first id-dict contains all
key-value pairs which passed the test, and the second contains the pairs that
did not.
-}
partition : (Int -> a -> Bool) -> Iddict a -> ( Iddict a, Iddict a )
partition f (Iddict d) =
    case Dict.partition f d.dict of
        ( p1, p2 ) ->
            ( Iddict { cursor = d.cursor, dict = p1 }
            , Iddict { cursor = d.cursor, dict = p2 }
            )

{-| Remove a key-value pair from the id-dict. If the key is not found, no
changes are made.
-}
remove : Int -> Iddict a -> Iddict a
remove k (Iddict d) =
    Iddict { d | dict = Dict.remove k d.dict }


{-| Create an id-dict with a single value.
-}
singleton : a -> ( Int, Iddict a )
singleton v =
    insert v empty


{-| Determine the number of key-value pairs in the id-dict.
-}
size : Iddict a -> Int
size (Iddict d) =
    Dict.size d.dict

{-| Convert an id-dict into an association list of key-value pairs, sorted by keys.
-}
toList : Iddict a -> List (Int, a)
toList (Iddict d) =
    Dict.toList d.dict

{-| Combine two id-dicts. If there is a collision, preference is given to the
first id-dict.
-}
union : Iddict a -> Iddict a -> Iddict a
union (Iddict d1) (Iddict d2) =
    Iddict
        { cursor = Basics.max d1.cursor d2.cursor
        , dict = Dict.union d1.dict d2.dict
        }

{-| Update the value of an id-dict for a specific key with a given function.
This is also the only option that allows you to insert values at a specific key,
so be careful with this feature!
-}
update : Int -> (Maybe a -> Maybe a) -> Iddict a -> Iddict a
update k f (Iddict d) =
    Iddict { d | dict = Dict.update k f d.dict }


{-| Get all of the values from an id-dict, in the order of their keys.
-}
values : Iddict a -> List a
values (Iddict { dict }) =
    Dict.values dict
