%% erlang module for using quickcheck to test multi_index

-module(multi_index_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).


%% Generator for a multi index. Right now we will only test tuples of ints.

multi_index(G) ->
    ?SIZED(Size, well_defined(multi_index(Size, G))).

multi_index(0, G) ->
    oneof([{call, multi_index, new, [indices(G)]},
            {call, multi_index, from_list, [list(G), indices(G)]}]);
multi_index(N, G) ->
    frequency([{5, multi_index(0, G)},
            {3, ?LAZY(?LETSHRINK([MI], [multi_index(N-1, G)],
                        {call, multi_index, insert, [G, MI]}))},
            {1, ?LAZY(?LETSHRINK([MI, I],
                        [multi_index(N-1, G), choose(1, size(G))],
                        {call, multi_index, erase, [element(I, G), I, MI]}))}
        ]).

indices(G) ->
    ?LET(N, choose(1, size(G)), return(lists:map(
                fun(X) -> {rand_index_type(), fun(V) -> element(X, V) end} end,
                        lists:seq(1, N)))).

rand_index_type() ->
    case random:uniform(2) of
        1 -> ordered_unique;
        2 -> ordered_non_unique
    end.

tuple_type() ->
    ?SIZED(Size, erlang:make_tuple((Size div 5) + 1, int())).


model(MI) ->
    Idxs = multi_index:indices(MI),
    {Idxs, lists:map(fun(N) -> lists:sort(multi_index:to_list(N, MI)) end,
            lists:seq(1, length(Idxs)))}.


indices_consistent(MI) ->
    NumIdx = length(multi_index:indices(MI)),
    [H | T] = lists:map(fun(N) -> lists:sort(multi_index:to_list(N, MI)) end,
        lists:seq(1, NumIdx)),
    lists:all(fun(L) -> H =:= L end, T).

prop_indices_consistent() -> prop_indices_consistent(tuple_type()).
prop_indices_consistent(G) ->
    ?FORALL(MI, multi_index(G), indices_consistent(eval(MI))).
