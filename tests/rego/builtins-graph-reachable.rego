package fregot.tests.builtins_graph_reachable

test_reachable_empty {
    graph.reachable({}, {"a"}) == set()
    graph.reachable({}, ["a"]) == set()
}

test_reachable_cycle {
    graph.reachable(
        {
            "a": {"b"},
            "b": {"c"},
            "c": {"a"},
        },
        {"a"}
    ) == {"a", "b", "c"}
}

test_reachable_components {
    graph.reachable(
        {
            "a": {"b", "c"},
            "b": {"d"},
            "c": {"d"},
            "d": set(),
            "e": {"f"},
            "f": {"e"},
            "x": {"x"},
        },
        {"b", "e"}
    ) == {"b", "d", "e", "f"}
}

test_reachable_arrays {
    graph.reachable(
        {
            "a": ["b"],
            "b": ["c"],
            "c": ["a"],
        },
        ["a"]
    ) == {"a", "b", "c"}
}
