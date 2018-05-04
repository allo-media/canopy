Canopy
======

A Generic Tree API, based on the [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree)
algebraic data type, augmented with helpers for building, querying and manipulating nodes.

Building a tree is basically done with using `node` and `leaf`:

```elm
tree : Node String
tree =
    node "root"
        [ node "foo"
            [ leaf "bar" ]
        , node "baz"
            [ leaf "qux" ]
        ]
```

The API focuses on node values for common operations:

```elm
tree
    |> remove "baz"
    |> append "foo" "bingo"
    == node "root"
        [ node "foo"
            [ leaf "bar"
            , leaf "bingo"
            ]
        ]
```

It exposes powerful helpers for working with tree data structures:

```elm
node 1
    [ node 2
        [ leaf 3, leaf 4 ]
    , node 5
        [ leaf 6 ]
    ]
    |> flatMap (value >> (*) 2)
    |> List.sum
    == 42
```

License
-------

MIT.
