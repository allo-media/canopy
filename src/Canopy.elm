module Canopy
    exposing
        ( Node(..)
        , all
        , any
        , append
        , children
        , decode
        , encode
        , filter
        , flatMap
        , flatten
        , foldl
        , foldr
        , fromList
        , get
        , getAll
        , leaf
        , leaves
        , length
        , level
        , map
        , mapChildren
        , mapChildrenAt
        , maximum
        , minimum
        , member
        , node
        , parent
        , path
        , prepend
        , remove
        , refine
        , replaceAt
        , replaceChildren
        , replaceChildrenAt
        , replaceValue
        , replaceValueAt
        , seed
        , seek
        , siblings
        , sortBy
        , sortWith
        , toList
        , tuple
        , updateAt
        , updateValue
        , updateValueAt
        , value
        , values
        )

{-| A generic [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree).

**Note:** This library is designed to work with unique values attached to tree nodes.
This is usually achievable using records having a unique id, though unicity checks should
be performed by consumers of this library.

@docs Node


# Building a tree

@docs node, leaf, append, prepend, remove, seed


# Querying a tree

@docs value, values, children, length, get, getAll, leaves, level, maximum, minimum, parent, path, seek, siblings


# Checking a tree

@docs all, any, member


# Manipulating a tree


## Single-level operations

@docs mapChildren, replaceChildren, replaceValue, updateValue


## Deep operations

@docs mapChildrenAt, replaceAt, replaceChildrenAt, replaceValueAt, updateAt , updateValueAt


## Common operations

@docs filter, flatMap, flatten, foldl, foldr, map, refine, sortBy, sortWith, tuple


# Importing and exporting

@docs fromList, toList, decode, encode

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree). Each node contains a
value and children, which are themselves Nodes:

    myTree : Tree Int
    myTree =
        Node 1
            [ Node 2
                [ Node 3 []
                , Node 4 []
                ]
            , Node 5 []
            ]

-}
type Node a
    = Node a (List (Node a))


{-| Check that all values satisfy a test in a tree.

    node 1 [ leaf 2 ]
        |> all (\x -> x > 0)
    --> True

    node 1 [leaf -2]
        |> all (\x -> x > 0)
    --> False

-}
all : (a -> Bool) -> Node a -> Bool
all test =
    values >> List.all test


{-| Check that any value satisfy a test in a tree.

    node 1 [ leaf -2 ]
        |> any (\x -> x > 0)
    --> True

    node -1 [ leaf -2 ]
        |> any (\x -> x > 0)
    --> False

-}
any : (a -> Bool) -> Node a -> Bool
any test =
    values >> List.any test


{-| Append a new value to a Node identified by its value in a Tree.

    node "foo" [ leaf "bar"]
        |> append "foo" "baz"
    --> node "foo" [ leaf "bar", leaf "baz" ]

-}
append : a -> a -> Node a -> Node a
append target child n =
    if target == value n then
        n |> replaceChildren (children n ++ [ leaf child ])
    else
        n |> mapChildren (append target child)


{-| Extracts the children of a Node.

    node "foo" [ leaf "bar" ]
        |> children
    --> [ leaf "bar" ]

-}
children : Node a -> List (Node a)
children (Node _ c) =
    c


{-| A JSON Node decoder. You must specify a decoder for values.

    import Json.Decode as Decode

    json : String
    json = "{\"value\":\"foo\",\"children\":[{\"value\":\"bar\",\"children\":[]}]}"

    Decode.decodeString (decode Decode.string) json
    --> Ok (node "foo" [ leaf "bar" ])

-}
decode : Decoder a -> Decoder (Node a)
decode decodeDatum =
    Decode.map2 Node
        (Decode.field "value" decodeDatum)
        (Decode.field "children" (Decode.list (Decode.lazy (\_ -> decode decodeDatum))))


{-| Encode a Node to JSON. You must provide an encoder for values.

    import Json.Encode as Encode

    node "foo" [ leaf "bar" ]
        |> encode Encode.string
        |> Encode.encode 0
    --> "{\"value\":\"foo\",\"children\":[{\"value\":\"bar\",\"children\":[]}]}"

-}
encode : (a -> Encode.Value) -> Node a -> Encode.Value
encode valueEncoder (Node v c) =
    Encode.object
        [ ( "value", valueEncoder v )
        , ( "children", Encode.list (encode valueEncoder) c )
        ]


{-| Filter a Tree strictly, removing all nodes failing the provided value test,
including root, hence the resulting Maybe.

    node 0 [ leaf 1 ]
        |> filter (\x -> x > 0)
    --> Nothing

    node 2 [ leaf 3, leaf 4 ]
        |> filter (\x -> modBy 2 x == 0)
    --> Just (node 2 [ leaf 4 ])

-}
filter : (a -> Bool) -> Node a -> Maybe (Node a)
filter test tree =
    if test (value tree) then
        let
            newChildren =
                tree
                    |> children
                    |> List.filter (value >> test)
                    |> List.filterMap (filter test)
        in
            tree |> replaceChildren newChildren |> Just
    else
        Nothing


{-| Map each node using a mapping function then flatten the result into a new list.

    node "foo" [ node "bar" [ leaf "baz" ] ]
        |> flatMap (value >> String.toUpper)
    --> [ "FOO", "BAR", "BAZ" ]

-}
flatMap : (Node a -> b) -> Node a -> List b
flatMap mapper tree =
    List.foldl
        (\n acc ->
            acc
                ++ mapper n
                :: (children n |> List.map (flatMap mapper) |> List.concat)
        )
        [ mapper tree ]
        (children tree)


{-| Flatten a Tree, from top to bottom and leftmost nodes to rightmost ones.

    node "root" [ node "foo" [ leaf "bar", leaf "baz" ] ]
        |> flatten
    --> [ node "root" [ node "foo" [ leaf "bar", leaf "baz" ] ]
    --> , node "foo" [ leaf "bar", leaf "baz" ]
    --> , leaf "bar"
    --> , leaf "baz"
    --> ]

-}
flatten : Node a -> List (Node a)
flatten =
    flatMap identity


{-| Reduce all tree values from top to bottom, left to right.

    node 1 [ node 2 [ leaf 3 ] ]
        |> foldl (+) 0
    --> 6

    node "a"
        [ node "b" [ leaf "c" ]
        , node "d" [ node "e" [ leaf "f" ] ]
        , leaf "g"
        ]
        |> foldl (\value acc -> acc ++ value) ""
    --> "abcdefg"

-}
foldl : (a -> b -> b) -> b -> Node a -> b
foldl fn acc =
    values >> List.foldl fn acc


{-| Reduce all tree values from top to bottom, right to left.

    node "a"
        [ node "b" [ leaf "c" ]
        , node "d" [ node "e" [ leaf "f" ] ]
        , leaf "g"
        ]
        |> foldr (\value acc -> acc ++ value) ""
    --> "gfedcba"

-}
foldr : (a -> b -> b) -> b -> Node a -> b
foldr fn acc =
    values >> List.foldr fn acc


{-| Build a tree from a list of hierarchy descriptors, which are tuples of value
and parent value, starting with the root.

    [ ( "root", Nothing )
    , ( "foo", Just "root" )
    , ( "bar", Just "foo" )
    ]
        |> fromList
    --> Just (node "root" [ node "foo" [ leaf "bar" ] ])

-}
fromList : List ( a, Maybe a ) -> Maybe (Node a)
fromList nodes =
    case List.head nodes of
        Just ( root, Nothing ) ->
            nodes
                |> List.foldl
                    (\( v, maybeParent ) acc ->
                        maybeParent
                            |> Maybe.map (\p -> append p v acc)
                            |> Maybe.withDefault acc
                    )
                    (leaf root)
                |> Just

        _ ->
            Nothing


{-| Get a Node holding a value from a tree, picking the first node found starting
from the left.

    node "root" [ leaf "bar" ]
        |> get "bar"
    --> Just (leaf "bar")

-}
get : a -> Node a -> Maybe (Node a)
get target =
    getAll target >> List.head


{-| Get all nodes containing the provided value.

    node 1 [ leaf 1 ]
        |> getAll 1
    --> [ node 1 [ leaf 1 ], leaf 1 ]

-}
getAll : a -> Node a -> List (Node a)
getAll target =
    flatten >> List.filter (value >> (==) target)


{-| Create a node having no children (singleton).

    leaf "foo"
    --> Node "foo" []

-}
leaf : a -> Node a
leaf v =
    Node v []


{-| Retrieve all leaves (singletons) from a tree.

    node "root"
        [ leaf "a leaf"
        , node "branch"
            [ leaf "another leaf" ]
        ]
        |> leaves
    --> [ "a leaf", "another leaf" ]

-}
leaves : Node a -> List a
leaves =
    flatten >> List.filter (children >> (==) []) >> List.map value


{-| Count nodes in a tree.

    node 1 [node 2 [ node 3 [ leaf 4 ] ] ]
    |> length
    --> 4

-}
length : Node a -> Int
length =
    foldl (\_ x -> x + 1) 0


{-| Retrieve all nodes at a given level in the tree.

    node "root"
        [ node "1" [ node "1.1" [ leaf "1.1.1" ] ]
        , node "2" [ node "2.1" [ leaf "2.1.1" ] ]
        ]
        |> level 3
    --> [ leaf "1.1.1"
    --> , leaf "2.1.1"
    --> ]

    node "root"
        [ node "1" [ node "1.1" [ leaf "1.1.1" ] ]
        , node "2" [ node "2.1" [ leaf "2.1.1" ] ]
        ]
        |> level 2
    --> [ node "1.1" [ leaf "1.1.1" ]
    --> , node "2.1" [ leaf "2.1.1" ]
    --> ]

-}
level : Int -> Node a -> List (Node a)
level lvl =
    if lvl <= 0 then
        List.singleton
    else
        children >> List.map (level (lvl - 1)) >> List.concat


{-| Map all Node values in a Tree.

    node "root" [ leaf "foo", node "bar" [ leaf "baz" ] ]
        |> map String.toUpper
    --> node "ROOT" [ leaf "FOO", node "BAR" [ leaf "BAZ" ] ]

-}
map : (a -> b) -> Node a -> Node b
map mapper (Node v c) =
    Node (mapper v) (c |> List.map (map mapper))


{-| Map a Node's children.

    node "root" [ leaf "foo", leaf "bar" ]
        |> mapChildren (map String.toUpper)
    --> node "root" [ leaf "FOO", leaf "BAR" ]

-}
mapChildren : (Node a -> Node a) -> Node a -> Node a
mapChildren mapper (Node v c) =
    Node v (List.map mapper c)


{-| Map a targetted Node's children in a tree.

    node 1 [ node 2 [ leaf 3, leaf 4 ] ]
        |> mapChildrenAt 2 (updateValue ((*) 2))
    --> node 1 [ node 2 [ leaf 6, leaf 8 ] ]

-}
mapChildrenAt : a -> (Node a -> Node a) -> Node a -> Node a
mapChildrenAt target mapper root =
    case get target root of
        Just n ->
            root |> replaceAt target (mapChildren mapper n)

        Nothing ->
            root


{-| Compute the maximum value appearing in a tree.

    node 1 [ leaf 100, node 2 [ leaf 3 ] ]
        |> maximum
    --> 100

-}
maximum : Node comparable -> comparable
maximum n =
    n |> values |> List.maximum |> Maybe.withDefault (value n)


{-| Compute the minimum value appearing in a tree.

    node 100 [ leaf 99, node 1 [ leaf 98 ] ]
        |> minimum
    --> 1

-}
minimum : Node comparable -> comparable
minimum n =
    n |> values |> List.minimum |> Maybe.withDefault (value n)


{-| Check if a tree contains a value.

    node "foo" [ node "bar" [ leaf "baz" ] ]
        |> member "baz"
    --> True

    leaf "no"
        |> member "yes"
    --> False

-}
member : a -> Node a -> Bool
member target =
    get target >> Maybe.map (always True) >> Maybe.withDefault False


{-| Create a Node. Basically just an alias for the `Node` constructor.
-}
node : a -> List (Node a) -> Node a
node =
    Node


{-| Retrieve the parent of a given node in a Tree, identified by its value.

    node "root" [ node "foo" [ leaf "bar" ] ]
        |> parent "bar"
    --> Just (node "foo" [ leaf "bar" ])

-}
parent : a -> Node a -> Maybe (Node a)
parent target candidate =
    candidate
        |> children
        |> List.foldl
            (\n acc ->
                case acc of
                    Just found ->
                        Just found

                    Nothing ->
                        if value n == target then
                            Just candidate
                        else
                            parent target n
            )
            Nothing


{-| Compute the path to a the first node holding the target value, from the root.

    node "root"
        [ leaf "qux"
        , node "foo"
            [ node "bar"
                [ leaf "baz" ]
            ]
        ]
        |> path "baz"
        |> List.map value
    --> [ "root", "foo", "bar", "baz" ]

-}
path : a -> Node a -> List (Node a)
path target rootNode =
    case ( get target rootNode, parent target rootNode ) of
        ( Just found, Nothing ) ->
            [ found ]

        ( Just found, Just p ) ->
            path (value p) rootNode ++ [ found ]

        _ ->
            []


{-| Prepend a new value to a Node identified by its value in a Tree.

    node "foo" [ leaf "bar"]
        |> prepend "foo" "baz"
    --> node "foo" [ leaf "baz", leaf "bar" ]

-}
prepend : a -> a -> Node a -> Node a
prepend target child n =
    if target == value n then
        n |> replaceChildren (leaf child :: children n)
    else
        n |> mapChildren (prepend target child)


{-| Deletes all occurrences of a value from a tree.

    node "root" [ node "foo" [ leaf "bar" ] ]
        |> remove "bar"
    --> node "root" [ leaf "foo" ]

Noop when the value doesn't exist in the tree:

    node "root" [ leaf "foo" ]
        |> remove "non-existent"
    --> node "root" [ leaf "foo" ]

Or when attempting to delete the tree itself:

    leaf "root"
        |> remove "root"
    --> leaf "root"

-}
remove : a -> Node a -> Node a
remove target tree =
    tree |> filter ((/=) target) |> Maybe.withDefault tree


{-| Filter a Tree, keeping only nodes which attached leaves satisfy the
provided test and preserving their ancestors, up to the tree root, which is
always kept.

    node 2
        [ node 4
            [ leaf 6
            , leaf 7
            , node 8
                [ leaf 10
                , leaf 11
                , leaf 12
                , leaf 13
                ]
            ]
        ]
        |> refine (\x -> modBy 2 x == 0)
    --> node 2
    -->    [ node 4
    -->        [ leaf 6
    -->        , node 8
    -->            [ leaf 10
    -->            , leaf 12
    -->            ]
    -->        ]
    -->    ]

-}
refine : (a -> Bool) -> Node a -> Node a
refine test tree =
    let
        toDelete =
            tree |> seek (not << test)

        toPreserve =
            tree
                |> seek test
                |> List.map (\(Node v _) -> tree |> path v |> List.map value)
                |> List.concat
    in
        toDelete
            |> List.filter (\(Node v _) -> List.member v toPreserve |> not)
            |> List.map value
            |> List.foldl remove tree


{-| Replace all Nodes holding a given value with a new Node in a Tree.

    node "root" [ node "foo" [ leaf "bar" ] ]
        |> replaceAt "foo" (leaf "bar")
    --> node "root" [ leaf "bar" ]

    node "root" [ leaf "foo", leaf "foo" ]
        |> replaceAt "foo" (leaf "bar")
    --> node "root" [ leaf "bar", leaf "bar" ]

-}
replaceAt : a -> Node a -> Node a -> Node a
replaceAt target replacement root =
    if value root == target then
        replacement
    else
        root |> mapChildren (replaceAt target replacement)


{-| Replace the children of a Node.
-}
replaceChildren : List (Node a) -> Node a -> Node a
replaceChildren c (Node v _) =
    Node v c


{-| Replace a targetted Node's children in a tree.

    node 1 [ node 2 [ leaf 3, leaf 4 ] ]
        |> replaceChildrenAt 2 [ leaf 5 ]
    --> node 1 [ node 2 [ leaf 5 ] ]

-}
replaceChildrenAt : a -> List (Node a) -> Node a -> Node a
replaceChildrenAt target c root =
    case get target root of
        Just n ->
            root |> replaceAt target (replaceChildren c n)

        Nothing ->
            root


{-| Replace the value of a Node.
-}
replaceValue : a -> Node a -> Node a
replaceValue v (Node _ c) =
    Node v c


{-| Replace a targetted Node value with another one in a Tree.

    node "root" [ node "foo" [ leaf "bar" ] ]
        |> replaceValueAt "foo" "baz"
    --> node "root" [ node "baz" [ leaf "bar" ] ]

    node "root" [ leaf "foo", leaf "foo" ]
        |> replaceValueAt "foo" "bar"
    --> node "root" [ leaf "bar", leaf "bar" ]

-}
replaceValueAt : a -> a -> Node a -> Node a
replaceValueAt target replacement root =
    case get target root of
        Just n ->
            root |> replaceAt target (replaceValue replacement n)

        Nothing ->
            root


{-| Retrieve all values from nodes containing those satisfying a provided condition.

    node 1 [ node 2 [ leaf 3, leaf 4, leaf 5 ] ]
        |> seek (\x -> x > 3)
    --> [ leaf 4, leaf 5 ]

-}
seek : (a -> Bool) -> Node a -> List (Node a)
seek test =
    flatten >> List.filter (value >> test)


{-| Seed a tree.

    seed (\x -> List.range 1 (x - 1)) 4
    --> node 4
    -->   [ leaf 1
    -->   , node 2 [ leaf 1 ]
    -->   , node 3
    -->       [ leaf 1
    -->       , node 2 [ leaf 1 ]
    -->       ]
    -->   ]

-}
seed : (a -> List a) -> a -> Node a
seed seeder init =
    Node init (seeder init |> List.map (seed seeder))


{-| Retrieve siblings of a node identified by its value in a Tree.

    node "foo" [ leaf "a", node "b" [ leaf "x" ], leaf "c" ]
        |> siblings "c"
    --> [ leaf "a", node "b" [ leaf "x" ] ]

-}
siblings : a -> Node a -> List (Node a)
siblings target tree =
    case parent target tree of
        Just (Node _ c) ->
            c |> List.filter (value >> (/=) target)

        Nothing ->
            []


{-| Recursively sort node children from a tree using a sorter.

    node 0 [ leaf 3, leaf 1, leaf 2 ]
        |> sortBy identity
    --> node 0 [ leaf 1, leaf 2, leaf 3 ]

-}
sortBy : (a -> comparable) -> Node a -> Node a
sortBy sorter (Node v c) =
    c
        |> List.sortBy (value >> sorter)
        |> List.map (sortBy sorter)
        |> Node v


{-| Recursively sort node children from a tree using a comparator.

    node 0 [ leaf 3, leaf 1, leaf 2 ]
        |> sortWith (\a b -> if a == b then EQ else if a < b then GT else LT)
    --> node 0 [ leaf 3, leaf 2, leaf 1 ]

-}
sortWith : (a -> a -> Order) -> Node a -> Node a
sortWith comparator (Node v c) =
    c
        |> List.sortWith (\a b -> comparator (value a) (value b))
        |> List.map (sortWith comparator)
        |> Node v


{-| Turn a tree of node into a list of tuples.

    node "root" [ node "foo" [ leaf "bar" ], leaf "baz" ]
        |> toList
    --> [ ( "root", Nothing )
    --> , ( "foo", Just "root")
    --> , ( "bar", Just "foo")
    --> , ( "baz", Just "root")
    --> ]

-}
toList : Node a -> List ( a, Maybe a )
toList n =
    n |> flatMap (tuple n)


{-| Turn a Node into a tuple containing the value and the parent value, if any.
-}
tuple : Node a -> Node a -> ( a, Maybe a )
tuple root n =
    ( value n, root |> parent (value n) |> Maybe.map value )


{-| Update a targetted Node in a tree, using an update function.

    node 1 [ node 2 [ leaf 3 ] ]
        |> updateAt 3 (always (leaf 42))
    --> node 1 [ node 2 [ leaf 42 ] ]

-}
updateAt : a -> (Node a -> Node a) -> Node a -> Node a
updateAt target update root =
    if value root == target then
        update root
    else
        root |> mapChildren (updateAt target update)


{-| Update a Node value using an update function. This is especially useful for
updating record properties.

    leaf { name = "root" }
        |> updateValue (\r -> { r | name = r.name ++ "!"})
    --> leaf { name = "root!" }

-}
updateValue : (a -> a) -> Node a -> Node a
updateValue update_ (Node v c) =
    Node (update_ v) c


{-| Update a targetted Node value in a tree, using an update function.

    node 1 [ node 2 [ leaf 3 ] ]
        |> updateValueAt 3 ((*) 2)
    --> node 1 [ node 2 [ leaf 6 ] ]

-}
updateValueAt : a -> (a -> a) -> Node a -> Node a
updateValueAt target update =
    updateAt target (updateValue update)


{-| Extract the value of a Node.

    leaf 2 |> value
    --> 2

-}
value : Node a -> a
value (Node v _) =
    v


{-| List all the values in a tree.

    node 1 [ node 2 [ leaf 3 ] ]
        |> values
    --> [ 1, 2, 3 ]

-}
values : Node a -> List a
values =
    flatten >> List.map value
