%% erlang module for using quickcheck to test multi_index ordered index

-module(ordered_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

identity(X) -> X.

ordered_non_unique_set(G) ->
    ?SIZED(Size, ordered_non_unique_set(Size, G)).

ordered_non_unique_set(0, G) ->
    oneof([{call, multi_index, ordered_new, []},
            {call, lists, foldl,
                [return(fun(X, A) -> multi_index:ordered_insert_non_unique(X,
                                    fun identity/1, A) end),
                    {call, multi_index, ordered_new, []}, list(G)]}]);
ordered_non_unique_set(N, G) ->
    frequency([{5, ordered_non_unique_set(N-1, G)},
            {3, ?LAZY(?LETSHRINK([Set], [ordered_non_unique_set(N-1, G)],
                        {call, multi_index, ordered_insert_non_unique,
                            [G, return(fun identity/1), Set]}))},
            {1, ?LAZY(?LETSHRINK([Set], [ordered_non_unique_set(N-1, G)],
                        {call, multi_index, ordered_erase_value_non_unique,
                            [G, return(fun identity/1), Set]}))}]).

ordered_unique_set(G) ->
    ?SIZED(Size, well_defined(ordered_unique_set(Size, G))).

ordered_unique_set(0, G) ->
    oneof([{call, multi_index, ordered_new, []},
            ?LET(L, list(G), {call, lists, foldl,
                    [return(fun(X, A) ->
                                    multi_index:ordered_insert_unique_or_throw(
                                        X, fun identity/1, A) end),
                        {call, multi_index, ordered_new, []},
                        lists:usort(L)]})]);
ordered_unique_set(N, G) ->
    frequency([{5, ordered_unique_set(0, G)},
            {3, ?LAZY(?LETSHRINK([Set], [ordered_unique_set(N-1, G)],
                        {call, multi_index, ordered_insert_unique_or_throw,
                            [?SUCHTHAT(X, G, multi_index:ordered_fetch_list(
                                        X, fun identity/1, eval(Set)) == []),
                                return(fun identity/1), Set]}))},
            {1, ?LAZY(?LETSHRINK([Set], [ordered_unique_set(N-1, G)],
                        {call, multi_index, ordered_erase_value_unique,
                            [G, return(fun identity/1), Set]}))}]).

model(S) ->
    lists:sort(multi_index:ordered_to_list(S)).

mfetch_or_throw(V, S) ->
    case lists:filter(fun(X) -> X == V end, S) of
        [] -> throw(badarg);
        [H | _] -> H
    end.

mfetch_list(V, S) ->
    lists:filter(fun(X) -> X == V end, S).

minsert_unique_or_throw(V, S) ->
    case lists:any(fun(X) -> X == V end, S) of
        true -> throw(badarg);
        false -> lists:sort([V | S])
    end.

minsert_non_unique(V, S) ->
    lists:sort([V | S]).

merase_value_unique(V, S) ->
    lists:filter(fun(X) -> X /= V end, S).

merase_value_non_unique(V, S) ->
    lists:filter(fun(X) -> X /= V end, S).

mreplace_value_unique_or_throw(V1, V2, S) ->
    S2 = lists:sort([V2 | lists:filter(fun(X) -> X /= V1 end, S)]),
    case S2 == lists:usort(S2) of
        true -> S2;
        false -> throw(badarg)
    end.

mreplace_value_non_unique(V1, V2, S) ->
    lists:sort(lists:map(fun(X) ->
                    if
                        X == V1 -> V2;
                        X /= V1 -> X
                    end end, S)).


prop_check_balance() -> prop_check_balance(int()).
prop_check_balance(G) ->
    ?FORALL(Set, oneof([ordered_non_unique_set(G), ordered_unique_set(G)]),
        multi_index:ordered_check_balance(eval(Set))).


prop_fetch_or_throw() -> prop_fetch_or_throw(int()).
prop_fetch_or_throw(G) ->
    ?FORALL({E, Set},
        {G, oneof([ordered_non_unique_set(G), ordered_unique_set(G)])},
        (catch multi_index:ordered_fetch_or_throw(E, fun identity/1,
                eval(Set))) ==
        (catch mfetch_or_throw(E, model(eval(Set))))).

prop_fetch_list() -> prop_fetch_list(int()).
prop_fetch_list(G) ->
    ?FORALL({E, Set},
        {G, oneof([ordered_non_unique_set(G), ordered_unique_set(G)])},
        multi_index:ordered_fetch_list(E, fun identity/1, eval(Set)) ==
        mfetch_list(E, model(eval(Set)))).

prop_insert_non_unique() -> prop_insert_non_unique(int()).
prop_insert_non_unique(G) ->
    ?FORALL({E, Set}, {G, ordered_non_unique_set(G)},
        model(multi_index:ordered_insert_non_unique(E, fun identity/1,
                eval(Set))) ==
        minsert_non_unique(E, model(eval(Set)))).


prop_insert_unique_or_throw() -> prop_insert_unique_or_throw(int()).
prop_insert_unique_or_throw(G) ->
    ?FORALL({E, Set}, {G, ordered_unique_set(G)},
        (catch model(multi_index:ordered_insert_unique_or_throw(E,
                    fun identity/1, eval(Set)))) ==
        (catch minsert_unique_or_throw(E, model(eval(Set))))).

prop_erase_value_non_unique() -> prop_erase_value_non_unique(int()).
prop_erase_value_non_unique(G) ->
    ?FORALL({E, Set}, {G, ordered_non_unique_set(G)},
        model(multi_index:ordered_erase_value_non_unique(E, fun identity/1,
                eval(Set))) ==
        merase_value_non_unique(E, model(eval(Set)))).

prop_erase_value_unique() -> prop_erase_value_unique(int()).
prop_erase_value_unique(G) ->
    ?FORALL({E, Set}, {G, ordered_unique_set(G)},
        (catch model(multi_index:ordered_erase_value_unique(E, fun identity/1,
                    eval(Set)))) ==
        (catch merase_value_unique(E, model(eval(Set))))).

prop_replace_value_non_unique() -> prop_replace_value_non_unique(int()).
prop_replace_value_non_unique(G) ->
    ?FORALL({E1, E2, Set}, {G, G, ordered_non_unique_set(G)},
        model(multi_index:ordered_replace_value_non_unique(E1, E2,
                fun identity/1, eval(Set))) ==
        mreplace_value_non_unique(E1, E2, model(eval(Set)))).

prop_replace_value_unique_or_throw() -> prop_replace_value_unique_or_throw(int()).
prop_replace_value_unique_or_throw(G) ->
    ?FORALL({E1, E2, Set}, {G, G, ordered_unique_set(G)},
        (catch model(multi_index:ordered_replace_value_unique_or_throw(E1, E2,
                    fun identity/1, eval(Set)))) ==
        (catch mreplace_value_unique_or_throw(E1, E2, model(eval(Set))))).
