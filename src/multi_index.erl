%%
%% Copyright (c) 2010, Gregory Rogers All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
%% THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
%% PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
%% EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%

-module(multi_index).

-export([erase/2, fetch/2, fetch_all/2, from_list/2, insert/2, new/1,
        replace/3, to_list/1, try_insert/2, view/2]).

-type key_fun() :: fun((term()) -> term()).

-record(multi_index, {
        indices = [] :: [ordered_unique | ordered_non_unique],
        key_funs = [] :: [key_fun()],
        key_val_stores = [] :: [gb_tree()]}).

-record(multi_index_view, {
        index :: ordered_unique | ordered_non_unique,
        key_fun :: key_fun(),
        key_val_store :: gb_tree(),
        view_of :: multi_index()}).

-type multi_index() :: #multi_index{}.
-type multi_index_view() :: #multi_index_view{}.
-type multi_index_option() :: {ordered_unique, key_fun()} |
    {ordered_non_unique, key_fun()}.



-spec erase(term(), multi_index() | multi_index_view()) -> multi_index().
erase(K, MIV) when is_record(MIV, multi_index_view) ->
    MI = MIV#multi_index_view.view_of,
    case fetch_all(K, MIV#multi_index_view.index,
            MIV#multi_index_view.key_val_store) of
        [] -> MI
        [V] -> MI#multi_index{
                key_val_stores = erase_one(V, MI#multi_index.indices,
                    MI#multi_index.key_funs, MI#multi_index.key_val_stores)};
        Vs -> MI#multi_index{
                key_val_stores = erase_all(
                    lists:foldl(fun sets:add_element/2, sets:new(), Vs),
                    MI#multi_index.indices, MI#multi_index.key_funs,
                    MI#multi_index.key_val_stores)};
    end;
erase(K, MI) when is_record(MI, multi_index) ->
    [Idx | _] = MI#multi_index.indices,
    [KVS | _] = MI#multi_index.key_val_stores,
    case fetch_all(K, Idx, KVS) of
        [] -> MI
        [V] -> MI#multi_index{
                key_val_stores = erase_one(V, MI#multi_index.indices,
                    MI#multi_index.key_funs, MI#multi_index.key_val_stores)};
        Vs -> MI#multi_index{
                key_val_stores = erase_all(
                    lists:foldl(fun sets:add_element/2, sets:new(), Vs),
                    MI#multi_index.indices, MI#multi_index.key_funs,
                    MI#multi_index.key_val_stores)};
    end.

erase_all(_, [], [], []) -> [];
erase_all(VSet, [ordered_unique | Idxs], [KF | KFs], [KVS | KVSs]) ->
    [sets:fold(fun(V, KVS2) -> gb_trees:delete(KF(V), KVS2) end, KVS, VSet) |
        erase_all(VSet, Idxs, KFs, KVSs)];
erase_all(VSet, [ordered_non_unique | Idxs], [KF | KFs], [KVS | KVSs]) ->
    KSet = sets:fold(fun(V, KSet2) -> sets:add_element(KF(V), KSet2) end,
        sets:new(), VSet),
    KVSNew = sets:fold(fun(K, KVS2) ->
                Vs = gb_trees:get(K, KVS2),
                case lists:filter(fun(V) ->
                                not sets:is_element(V, VSet) end, Vs) of
                    [] -> KVS2;
                    VsNew -> gb_trees:update(K, VsNew, KVS2)
                end
        end, KVS, KSet),
    [KVSNew | erase_all(VSet, Idxs, KFs, KVSs)].

erase_one(_, [], [], []) -> [];
erase_one(V, [ordered_unique | Idxs], [KF | KFs], [KVS | KVSs]) ->
    [gb_trees:delete(KF(V), KVS) | erase_one(V, Idxs, KFs, KVSs)];
erase_one(V, [ordered_non_unique | Idxs], [KF | KFs], [KVS | KVSs]) ->
    K = KF(V),
    KVSNew = case lists:filter(fun(X) -> X =/= V end, gb_trees:get(K, KVS)) of
        [] -> gb_trees:delete(K, KVS);
        Vs -> gb_trees:update(K, Vs, KVS)
    end,
    [KVSNew | erase_one(V, Idxs, KFs, KVSs)].


-spec fetch(term(), multi_index() | multi_index_view()) -> term().
fetch(K, MIV) when is_record(MIV, multi_index_view) ->
    fetch_one(K, MIV#multi_index_view.index,
        MIV#multi_index_view.key_val_store);
fetch(K, MI) when is_record(MI, multi_index) ->
    [Idx | _] = MI#multi_index.indices,
    [KVS | _] = MI#multi_index.key_val_stores,
    fetch_one(K, Idx, KVS).

fetch_one(K, ordered_unique, KVS) ->
    gb_trees:get(K, KVS);
fetch_one(K, ordered_non_unique, KVS) ->
    [V | _] = gb_trees:get(K, KVS),
    V.


-spec fetch_all(term(), multi_index() | multi_index_view()) -> [term()].
fetch_all(K, MIV) when is_record(MIV, multi_index_view) ->
    fetch_all(K, MIV#multi_index_view.index,
        MIV#multi_index_view.key_val_store);
fetch_all(K, MI) when is_record(MI, multi_index) ->
    [Idx | _] = MI#multi_index.indices,
    [KVS | _] = MI#multi_index.key_val_stores,
    fetch_all(K, Idx, KVS).

fetch_all(K, ordered_unique, KVS) ->
    case gb_trees:lookup(K, KVS) of
        {value, V} -> [V];
        none -> []
    end;
fetch_all(K, ordered_non_unique, KVS) ->
    case gb_trees:lookup(K, KVS) of
        {value, Vs} -> Vs;
        none -> []
    end.


-spec from_list([term()], [multi_index_option()]) -> multi_index().
from_list(Vs, Opts) ->
    lists:foldl(fun insert/2, new(Opts), Vs).


-spec insert(term(), multi_index()) -> multi_index().
insert(Val, MI) ->
    MI#multi_index{
        key_val_stores = insert_one(Val, MI#multi_index.indices,
            MI#multi_index.key_funs, MI#multi_index.key_val_stores)}.

insert_one(_V, [], [], []) -> [];
insert_one(V, [ordered_unique | Idxs], [KF | KFs], [KVS | KVSs]) ->
    [gb_trees:insert(KF(V), V, KVS) |
        insert_one(V, Idxs, KFs, KVSs)];
insert_one(V, [ordered_non_unique | Idxs], [KF | KFs], [KVS | KVSs]) ->
    K = KF(V),
    case gb_trees:lookup(K, KVS) of
        {value, Vals} ->
            [gb_trees:update(K, [V | Vals], KVS) |
                insert_one(V, Idxs, KFs, KVSs)];
        none ->
            [gb_trees:insert(K, [V], KVS) | insert_one(V, Idxs, KFs, KVSs)]
    end.


-spec new([multi_index_option()]) -> multi_index().
new(Opts) ->
    MI = lists:foldl(fun add_option/2, #multi_index{}, Opts),
    #multi_index{
        indices = lists:reverse(MI#multi_index.indices),
        key_funs = lists:reverse(MI#multi_index.key_funs),
        key_val_stores = lists:reverse(MI#multi_index.key_val_stores)}.

add_option({ordered_unique, KeyFun}, MI) ->
    #multi_index{
        indices = [ordered_unique | MI#multi_index.indices],
        key_funs = [KeyFun | MI#multi_index.key_funs],
        key_val_stores = [gb_trees:empty() | MI#multi_index.key_val_stores]};
add_option({ordered_non_unique, KeyFun}, MI) ->
    #multi_index{
        indices = [ordered_non_unique | MI#multi_index.indices],
        key_funs = [KeyFun | MI#multi_index.key_funs],
        key_val_stores = [gb_trees:empty() | MI#multi_index.key_val_stores]}.


-spec replace(term(), term(), multi_index()) -> multi_index().
replace(V1, V2, MI) when is_record(MI, multi_index) ->
    MI#multi_index{key_val_stores = replace(V1, V2, MI#multi_index.indices,
            MI#multi_index.key_funs, MI#multi_index.key_val_stores)}.

replace(_V1, _V2, [], [], []) -> {ok, []};
replace(V1, V2, [ordered_unique | Idxs], [KF | KFs], [KVS | KVSs]) ->
    K = KF(V1),
    case gb_trees:lookup(K, KVS) of
        {value, V1} ->
            [gb_trees:update(K, V2, KVS) | replace(V1, V2, Idxs, KFs, KVSs)];
        _ -> erlang:error(badarg)
    end;
replace(V1, V2, [ordered_non_unique | Idxs], [KF | KFs], [KVS | KVSs]) ->
    K = KF(V1),
    case gb_trees:lookup(K, KVS) of
        {value, Vs} ->
            case lists:mapfoldl(fun(V, E) ->
                            case V =:= V1 of
                                true -> {V2, ok};
                                false -> {V, E}
                            end end, error, Vs) of
                {VsNew, ok} ->
                    [gb_trees:update(K, VsNew, KVS) |
                        replace(V1, V2, Idxs, KFs, KVSs)];
                {_, error} -> erlang:error(badarg)
            end;
        none -> erlang:error(badarg)
    end.


-spec to_list(multi_index() | multi_index_view()) -> [term()].
to_list(MIV) when is_record(MIV, multi_index_view) ->
    to_list(MIV#multi_index_view.index, MIV#multi_index_view.key_val_store);
to_list(MI) when is_record(MI, multi_index) ->
    [Idx | _] = MI#multi_index.indices,
    [KVS | _] = MI#multi_index.key_val_stores,
    to_list(Idx, KVS).

to_list(ordered_unique, KVS) ->
    gb_trees:values(KVS);
to_list(ordered_non_unique, KVS) ->
    lists:flatmap(fun(X) -> X end, gb_trees:values(KVS)).


-spec try_insert(term(), multi_index()) -> {'ok', multi_index()} | 'error'.
try_insert(Val, MI) ->
    case try_insert_one(Val, MI#multi_index.indices, MI#multi_index.key_funs,
            MI#multi_index.key_val_stores) of
        error -> error;
        {ok, KVSs} -> {ok, MI#multi_index{key_val_stores = KVSs}}
    end.

try_insert_one(_V, [], [], []) -> {ok, []};
try_insert_one(V, [ordered_unique | Idxs], [KF | KFs], [KVS | KVSs]) ->
    case try_insert_one(V, Idxs, KFs, KVSs) of
        {ok, KVSsNew} ->
            K = KF(V),
            case gb_trees:lookup(K, KVS) of
                {value, _} -> error;
                none -> {ok, [gb_trees:insert(K, V, KVS) | KVSsNew]}
            end;
        error -> error
    end;
try_insert_one(V, [ordered_non_unique | Idxs], [KF | KFs], [KVS | KVSs]) ->
    case try_insert_one(V, Idxs, KFs, KVSs) of
        {ok, KVSsNew} ->
            K = KF(V),
            case gb_trees:lookup(K, KVS) of
                {value, Vals} ->
                    {ok, [gb_trees:update(K, [V | Vals], KVS) | KVSsNew]};
                none ->
                    {ok, [gb_trees:insert(K, [V], KVS) | KVSsNew]}
            end;
        error -> error
    end.


-spec view(integer(), multi_index()) -> multi_index_view().
view(Num, MI) when is_record(MI, multi_index), is_integer(Num), Num > 0,
        Num =< length(MI#multi_index.indices) ->
    #multi_index_view{
        index = lists:nth(Num, MI#multi_index.indices),
        key_fun = lists:nth(Num, MI#multi_index.key_funs),
        key_val_store = lists:nth(Num, MI#multi_index.key_val_stores),
        view_of = MI}.
