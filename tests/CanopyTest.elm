module CanopyTest exposing (..)

import Canopy exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Expect exposing (Expectation)
import Test exposing (..)


asTest : String -> Expectation -> Test
asTest label expectation =
    expectation |> always |> test label


testTree : Node String
testTree =
    node
        "root"
        [ leaf "node 1"
        , node
            "node 2"
            [ leaf "node 2.1"
            , leaf "node 2.2"
            , leaf "node 2.3"
            ]
        , leaf "node 3"
        ]


json : String
json =
    """{
  "value": "root",
  "children": [
    {
      "value": "node 1",
      "children": []
    },
    {
      "value": "node 2",
      "children": [
        {
          "value": "node 2.1",
          "children": []
        },
        {
          "value": "node 2.2",
          "children": []
        },
        {
          "value": "node 2.3",
          "children": []
        }
      ]
    },
    {
      "value": "node 3",
      "children": []
    }
  ]
}"""


testAll : Test
testAll =
    describe "all"
        [ node 1 [ leaf 2 ]
            |> all (\x -> x > 0)
            |> Expect.equal True
            |> asTest "should ensure all values match a test"
        , node 0 [ leaf 2 ]
            |> all (\x -> x > 0)
            |> Expect.equal False
            |> asTest "should return False when a test doesn't pass"
        ]


testAny : Test
testAny =
    describe "any"
        [ node 0 [ leaf 2 ]
            |> any (\x -> x > 0)
            |> Expect.equal True
            |> asTest "should ensure any values match a test"
        , node 0 [ leaf -2 ]
            |> any (\x -> x > 0)
            |> Expect.equal False
            |> asTest "should return False when a test doesn't pass"
        ]


testAppendChild : Test
testAppendChild =
    describe "append"
        [ node "foo" [ leaf "bar" ]
            |> append "foo" "baz"
            |> Expect.equal (node "foo" [ leaf "bar", leaf "baz" ])
            |> asTest "should append a child to a node"
        , node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> append "qux" "boo"
            |> Expect.equal (node "foo" [ leaf "bar", node "baz" [ node "qux" [ leaf "boo" ] ] ])
            |> asTest "should deeply append a child to a node"
        , node "foo" [ leaf "bar" ]
            |> append "non-existent" "baz"
            |> Expect.equal (node "foo" [ leaf "bar" ])
            |> asTest "should not append a node to a non-existent parent"
        ]


testBuildingAPI : Test
testBuildingAPI =
    describe "building API"
        [ node "root"
            [ node "foo"
                [ leaf "bar" ]
            , node "baz"
                [ leaf "qux" ]
            ]
            |> remove "baz"
            |> append "foo" "bingo"
            |> Expect.equal
                (node "root"
                    [ node "foo"
                        [ leaf "bar"
                        , leaf "bingo"
                        ]
                    ]
                )
            |> asTest "should allow building a tree"
        ]


testDecode : Test
testDecode =
    describe "decode"
        [ json
            |> Decode.decodeString (decode Decode.string)
            |> Expect.equal (Ok testTree)
            |> asTest "should decode a tree"
        ]


testEncode : Test
testEncode =
    describe "encode"
        [ testTree
            |> encode Encode.string
            |> Encode.encode 2
            |> Expect.equal json
            |> asTest "should encode a tree"
        ]


testFilter : Test
testFilter =
    describe "filter"
        [ node 0 [ leaf 1 ]
            |> filter (\x -> x > 0)
            |> Expect.equal Nothing
            |> asTest "should filter out a non-matching root node"
        , node 2 [ leaf 3, leaf 4 ]
            |> filter (\x -> modBy 2 x == 0)
            |> Expect.equal (Just (node 2 [ leaf 4 ]))
            |> asTest "should filter a simple tree"
        , node 2 [ leaf 3, leaf 4, node 5 [ leaf 6 ] ]
            |> filter (\x -> modBy 2 x == 0)
            |> Expect.equal (Just (node 2 [ leaf 4 ]))
            |> asTest "should filter a deep tree"
        ]


testFlatMap : Test
testFlatMap =
    describe "flatMap"
        [ testTree
            |> flatMap (value >> String.toUpper)
            |> Expect.equal
                [ "ROOT"
                , "NODE 1"
                , "NODE 2"
                , "NODE 2.1"
                , "NODE 2.2"
                , "NODE 2.3"
                , "NODE 3"
                ]
            |> asTest "should flatMap a tree"
        ]


testFlatten : Test
testFlatten =
    describe "flatten"
        [ testTree
            |> flatten
            |> List.map value
            |> Expect.equal
                [ "root"
                , "node 1"
                , "node 2"
                , "node 2.1"
                , "node 2.2"
                , "node 2.3"
                , "node 3"
                ]
            |> asTest "should flatten a tree"
        , node "root" [ node "foo" [ leaf "bar", leaf "baz" ] ]
            |> flatten
            |> List.map value
            |> Expect.equal [ "root", "foo", "bar", "baz" ]
            |> asTest "should have code docs sample working"
        , node 1
            [ node 2
                [ leaf 3
                , node 4
                    [ leaf 5
                    , leaf 6
                    , leaf 7
                    , leaf 8
                    ]
                ]
            ]
            |> flatten
            |> Expect.equal
                [ Node 1 ([ Node 2 ([ Node 3 [], Node 4 ([ Node 5 [], Node 6 [], Node 7 [], Node 8 [] ]) ]) ])
                , Node 2 ([ Node 3 [], Node 4 ([ Node 5 [], Node 6 [], Node 7 [], Node 8 [] ]) ])
                , Node 3 []
                , Node 4 ([ Node 5 [], Node 6 [], Node 7 [], Node 8 [] ])
                , Node 5 []
                , Node 6 []
                , Node 7 []
                , Node 8 []
                ]
            |> asTest "should work with integers"
        , node 1
            [ node 2
                [ leaf 3, leaf 4 ]
            , node 5
                [ leaf 6 ]
            ]
            |> flatMap (value >> (*) 2)
            |> List.sum
            |> Expect.equal 42
            |> asTest "should work with integer 2"
        ]


testFromList : Test
testFromList =
    describe "fromList"
        [ [ ( "root", Nothing )
          , ( "node 1", Just "root" )
          , ( "node 2", Just "root" )
          , ( "node 2.1", Just "node 2" )
          , ( "node 2.2", Just "node 2" )
          , ( "node 2.3", Just "node 2" )
          , ( "node 3", Just "root" )
          ]
            |> fromList
            |> Expect.equal (Just testTree)
            |> asTest "should build a tree from a list"
        , testTree
            |> toList
            |> fromList
            |> Expect.equal (Just testTree)
            |> asTest "should be idempotent"
        ]


testGet : Test
testGet =
    describe "get"
        [ testTree
            |> get "root"
            |> Expect.equal (Just testTree)
            |> asTest "should find root node"
        , testTree
            |> get "node 2.3"
            |> Expect.equal (Just (leaf "node 2.3"))
            |> asTest "should find a deeply nested node"
        , testTree
            |> get "non-existent"
            |> Expect.equal Nothing
            |> asTest "should not find a non-existent node"
        , node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> get "baz"
            |> Expect.equal (Just (node "baz" [ leaf "qux" ]))
            |> asTest "hello"
        , Node "foo" ([ Node "bar" [], Node "baz" ([ Node "qux" [] ]) ])
            |> get "bar"
            |> Expect.equal (Just (leaf "bar"))
            |> asTest "hello 2"
        ]


testLeaves : Test
testLeaves =
    describe "leaves"
        [ testTree
            |> leaves
            |> Expect.equal
                [ "node 1"
                , "node 2.1"
                , "node 2.2"
                , "node 2.3"
                , "node 3"
                ]
            |> asTest "should retrieve all tree leaves"
        , node 1
            [ node 2
                [ leaf 3
                , node 4
                    [ leaf 5
                    , leaf 6
                    , leaf 7
                    , leaf 8
                    ]
                ]
            ]
            |> leaves
            |> Expect.equal [ 3, 5, 6, 7, 8 ]
            |> asTest "should work in a deeply fashion"
        ]


testLevel : Test
testLevel =
    describe "level"
        [ testTree
            |> level 0
            |> Expect.equal [ testTree ]
            |> asTest "should retrieve root level"
        , testTree
            |> level 1
            |> List.map value
            |> Expect.equal [ "node 1", "node 2", "node 3" ]
            |> asTest "should retrieve level 1 nodes"
        , testTree
            |> level 2
            |> List.map value
            |> Expect.equal [ "node 2.1", "node 2.2", "node 2.3" ]
            |> asTest "should retrieve level 2 nodes"
        , testTree
            |> level -999
            |> Expect.equal [ testTree ]
            |> asTest "should handle negative levels"
        ]


testMap : Test
testMap =
    describe "map"
        [ testTree
            |> map String.toUpper
            |> Expect.equal
                (node
                    "ROOT"
                    [ leaf "NODE 1"
                    , node
                        "NODE 2"
                        [ leaf "NODE 2.1"
                        , leaf "NODE 2.2"
                        , leaf "NODE 2.3"
                        ]
                    , leaf "NODE 3"
                    ]
                )
            |> asTest "should map a tree"
        ]


testMaximum : Test
testMaximum =
    describe "maximum"
        [ node 4 [ node 3 [ leaf 1 ], node 2 [ leaf 5 ] ]
            |> maximum
            |> Expect.equal 5
            |> asTest "should compute the maximum comparable value of a tree"
        , node -4 [ node -3 [ leaf -1 ], node -2 [ leaf -5 ] ]
            |> maximum
            |> Expect.equal -1
            |> asTest "should handle negative values"
        ]


testMinimum : Test
testMinimum =
    describe "minimum"
        [ node 4 [ node 3 [ leaf 1 ], node 2 [ leaf 5 ] ]
            |> minimum
            |> Expect.equal 1
            |> asTest "should compute the minimum comparable value of a tree"
        , node -4 [ node -3 [ leaf -1 ], node -2 [ leaf -5 ] ]
            |> minimum
            |> Expect.equal -5
            |> asTest "should handle negative values"
        ]


testNode : Test
testNode =
    describe "node"
        [ node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> Expect.equal (Node "foo" ([ Node "bar" [], Node "baz" ([ Node "qux" [] ]) ]))
            |> asTest "should create a node"
        ]


testParent : Test
testParent =
    describe "parent"
        [ testTree
            |> parent "node 2.3"
            |> Maybe.map value
            |> Expect.equal (Just "node 2")
            |> asTest "should find the parent of a given node"
        , testTree
            |> parent "node 2"
            |> Maybe.map value
            |> Expect.equal (Just "root")
            |> asTest "should find the parent when it's root"
        , testTree
            |> parent "root"
            |> Expect.equal Nothing
            |> asTest "should not find any parent for root"
        ]


testPath : Test
testPath =
    describe "path"
        [ testTree
            |> path "non-existent"
            |> Expect.equal []
            |> asTest "should return an empty path to a non-existent node"
        , testTree
            |> path "root"
            |> List.map value
            |> Expect.equal [ "root" ]
            |> asTest "should compute the path to root"
        , testTree
            |> path "node 2"
            |> List.map value
            |> Expect.equal [ "root", "node 2" ]
            |> asTest "should compute the path to reach a node"
        , testTree
            |> path "node 2.2"
            |> List.map value
            |> Expect.equal [ "root", "node 2", "node 2.2" ]
            |> asTest "should compute the path to reach a deeply nested node"

        -- handcrafted tree
        , node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> path "bar"
            |> Expect.equal
                [ node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
                , leaf "bar"
                ]
            |> asTest "should find the path to a leaf at the first level"
        , node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> path "baz"
            |> Expect.equal
                [ node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
                , node "baz" [ leaf "qux" ]
                ]
            |> asTest "should find the path to a node at the first level"
        , node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> path "qux"
            |> Expect.equal
                [ node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
                , node "baz" [ leaf "qux" ]
                , leaf "qux"
                ]
            |> asTest "should find the path to a leaf at the second level"
        ]


testPrependChild : Test
testPrependChild =
    describe "prepend"
        [ node "foo" [ leaf "bar" ]
            |> prepend "foo" "baz"
            |> Expect.equal (node "foo" [ leaf "baz", leaf "bar" ])
            |> asTest "should prepend a child to a node"
        , node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> prepend "qux" "boo"
            |> Expect.equal (node "foo" [ leaf "bar", node "baz" [ node "qux" [ leaf "boo" ] ] ])
            |> asTest "should deeply prepend a child to a node"
        , node "foo" [ leaf "bar" ]
            |> prepend "non-existent" "baz"
            |> Expect.equal (node "foo" [ leaf "bar" ])
            |> asTest "should not prepend a node to a non-existent parent"
        ]


testRefine : Test
testRefine =
    describe "refine"
        [ testTree
            |> refine (always True)
            |> Expect.equal testTree
            |> asTest "should noop refine a tree"
        , testTree
            |> refine (\s -> String.length s > 4)
            |> Expect.equal testTree
            |> asTest "should never refine out tree root"
        , testTree
            |> refine (String.contains "2")
            |> flatMap value
            |> Expect.equal [ "root", "node 2", "node 2.1", "node 2.2", "node 2.3" ]
            |> asTest "should selectively refine tree nodes"
        , testTree
            |> refine ((==) "node 2.2")
            |> flatMap value
            |> Expect.equal [ "root", "node 2", "node 2.2" ]
            |> asTest "should preserve ancestors"
        ]


testRemove : Test
testRemove =
    describe "remove"
        [ testTree
            |> remove "node 2.1"
            |> flatMap value
            |> Expect.equal [ "root", "node 1", "node 2", "node 2.2", "node 2.3", "node 3" ]
            |> asTest "should delete a deeply nested node from a tree"
        , testTree
            |> remove "node 2"
            |> flatMap value
            |> Expect.equal [ "root", "node 1", "node 3" ]
            |> asTest "should delete a node from a tree"
        , testTree
            |> remove "root"
            |> Expect.equal testTree
            |> asTest "should not delete tree root"
        ]


testReplaceAt : Test
testReplaceAt =
    describe "replaceAt"
        [ testTree
            |> replaceAt "node 2.2" (leaf "blah")
            |> get "blah"
            |> Expect.equal (Just (leaf "blah"))
            |> asTest "should replace a node in the tree"
        , testTree
            |> replaceAt "node 2" (node "node 2" [ leaf "blah" ])
            |> get "node 2"
            |> Expect.equal (Just (node "node 2" [ leaf "blah" ]))
            |> asTest "should replace a node and children in the tree"
        ]


testReplaceValueAt : Test
testReplaceValueAt =
    describe "replaceValueAt"
        [ testTree
            |> replaceValueAt "node 2.2" "blah"
            |> get "blah"
            |> Expect.equal (Just (leaf "blah"))
            |> asTest "should replace a node in the tree"
        ]


testSeek : Test
testSeek =
    describe "seek"
        [ testTree
            |> seek (String.contains ".")
            |> List.map value
            |> Expect.equal [ "node 2.1", "node 2.2", "node 2.3" ]
            |> asTest "should seek a tree"
        , testTree
            |> seek (String.startsWith "node")
            |> List.map value
            |> Expect.equal [ "node 1", "node 2", "node 2.1", "node 2.2", "node 2.3", "node 3" ]
            |> asTest "should seek a tree 2"
        ]


testSiblings : Test
testSiblings =
    describe "siblings"
        [ testTree
            |> siblings "node 2.2"
            |> Expect.equal [ leaf "node 2.1", leaf "node 2.3" ]
            |> asTest "should retrieve node siblings across the tree"
        ]


testSortBy : Test
testSortBy =
    describe "sortBy"
        [ testTree
            |> map String.reverse
            |> sortBy identity
            |> Expect.equal
                (node "toor"
                    [ leaf "1 edon"
                    , node "2 edon" [ leaf "1.2 edon", leaf "2.2 edon", leaf "3.2 edon" ]
                    , leaf "3 edon"
                    ]
                )
            |> asTest "should sort a tree"
        ]


testSortWith : Test
testSortWith =
    describe "sortWith"
        [ node "a" [ leaf "aa", node "aaa" [ leaf "aaaa", leaf "aaaaa" ] ]
            |> sortWith
                (\a b ->
                    if String.length a == String.length b then
                        EQ
                    else if String.length a < String.length b then
                        GT
                    else
                        LT
                )
            |> Expect.equal
                (node "a" [ node "aaa" [ leaf "aaaaa", leaf "aaaa" ], leaf "aa" ])
            |> asTest "should sort a tree"
        ]


testToList : Test
testToList =
    describe "toList"
        [ testTree
            |> toList
            |> Expect.equal
                [ ( "root", Nothing )
                , ( "node 1", Just "root" )
                , ( "node 2", Just "root" )
                , ( "node 2.1", Just "node 2" )
                , ( "node 2.2", Just "node 2" )
                , ( "node 2.3", Just "node 2" )
                , ( "node 3", Just "root" )
                ]
            |> asTest "should turn a node into a list of tuples"
        ]


testTuple : Test
testTuple =
    describe "tuple"
        [ testTree
            |> tuple testTree
            |> Expect.equal ( "root", Nothing )
            |> asTest "should map a root node to a tuple"
        , testTree
            |> get "node 2.2"
            |> Maybe.map (tuple testTree)
            |> Expect.equal (Just ( "node 2.2", Just "node 2" ))
            |> asTest "should map a nested node to a tuple"
        ]


testUpdateAt : Test
testUpdateAt =
    describe "updateAt"
        [ node 1 [ node 2 [ leaf 3 ] ]
            |> updateAt 3 (\node -> leaf (value node * 2))
            |> Expect.equal (node 1 [ node 2 [ leaf 6 ] ])
            |> asTest "should update a node at a given value"
        ]


testUpdateValueAt : Test
testUpdateValueAt =
    describe "updateValueAt"
        [ node 1 [ leaf 2 ]
            |> updateValueAt 1 (always 42)
            |> Expect.equal (node 42 [ leaf 2 ])
            |> asTest "should update a node value"
        ]
