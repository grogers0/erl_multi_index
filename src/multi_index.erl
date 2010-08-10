%%
%% %CopyrightBegin%
%%
%% Copyright Gregory Rogers, 2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(multi_index).

-compile(export_all).

-type key_fun() :: fun((term()) -> term()).

-record(multi_index, {
        indices = [] :: [ordered_unique | ordered_non_unique],
        key_funs = [] :: [fun((term()) -> term())],
        key_val_stores = [] :: [gb_tree()]}).

-record(multi_index_view, {
        index :: ordered_unique | ordered_non_unique,
        key_fun :: fun((term()) -> term()),
        key_val_store :: gb_tree()}).

-type multi_index() :: #multi_index{}.
-type multi_index_view() :: #multi_index_view{}.
-type multi_index_option() :: {ordered_unique, key_fun()} |
    {ordered_non_unique, key_fun()}.



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

-spec new([multi_index_option()]) -> multi_index().
new(Opts) ->
    MI = lists:foldl(fun add_option/2, #multi_index{}, Opts),
    #multi_index{
        indices = lists:reverse(MI#multi_index.indices),
        key_funs = lists:reverse(MI#multi_index.key_funs),
        key_val_stores = lists:reverse(MI#multi_index.key_val_stores)}.

add_option({IndexType, KeyFun}, MI) ->
    #multi_index{
        indices = [IndexType | MI#multi_index.indices],
        key_funs = [KeyFun | MI#multi_index.key_funs],
        key_val_stores = [gb_trees:empty() | MI#multi_index.key_val_stores]}.

-spec store(term(), multi_index()) -> multi_index().
store(Val, MI) ->
    MI#multi_index{
        key_val_stores = store_one(Val, MI#multi_index.indices,
            MI#multi_index.key_funs, MI#multi_index.key_val_stores)}.

store_one(_V, [], [], []) -> [];
store_one(V, [ordered_unique | Idxs], [KF | KFs], [KVS | KVSs]) ->
    [gb_trees:insert(KF(V), V, KVS) |
        store_one(V, Idxs, KFs, KVSs)];
store_one(V, [ordered_non_unique | Idxs], [KF | KFs], [KVS | KVSs]) ->
    K = KF(V),
    case gb_trees:lookup(K, KVS) of
        {value, Vals} ->
            [gb_trees:update(K, [V | Vals], KVS) |
                store_one(V, Idxs, KFs, KVSs)];
        none ->
            [gb_trees:insert(K, [V], KVS) | store_one(V, Idxs, KFs, KVSs)]
    end.
