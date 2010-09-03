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
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%%

%% @author Gregory Rogers <greg.r.rogers@gmail.com>

%% @doc Multi index implements a key - value dictionary which stores values
%% based on multiple keys. Multi index only stores values, the keys used for
%% indices are derived from the values by user defined key functions. There can
%% be multiple indices, and it is possible to look up values based on any of
%% these indices. A common use case is to store tuples, which can be looked up
%% by each of their elements.
%% 
%% When creating indices, an <i>index type</i> and a <i>key function</i> are
%% needed. The key function takes a value and generates a key from it to be
%% used in indexing the values.  There are two types of indices available -
%% <i>ordered_unique</i> and <i>ordered_non_unique</i>.  Both of these indices
%% store values in order based on the key. With an ordered_unique index, keys
%% derived from values cannot compare the same as any other key for that index,
%% this comparison is done with <code>==</code> operator. When calling
%% functions that take an <i>IndexNum</i>, the index num is the position in the
%% list of indices given to {@link new/1}, for example in the multi index
%% <code>MI = multi_index:new([{ordered_non_unique, fun x/1}, {ordered_unique,
%% fun y/1}])</code> the non-unique index with function x is index 1, and the
%% unique index with function y is index 2.
%%
%% == EXAMPLES ==
%% Example of an employee list using a multi index to look up employees by
%% various fields.
%% <code><pre>
%% -module(employee_list).
%% 
%% -define(BY_NAME, 1).
%% -define(BY_JOB, 2).
%% 
%% -export([new/0, hire/3, fire/2, department_list/2]).
%% 
%% new() ->
%%     multi_index:new([{ordered_unique, fun(X) -> element(1, X) end},
%%             {ordered_non_unique, fun(X) -> element(2, X) end}]).
%% 
%% hire(Name, Job, Employees) ->
%%     case multi_index:try_insert({Name, Job}, Employees) of
%%         {ok, NewEmployees} -> {ok, NewEmployees};
%%         error -> {error, "This employee already works here"}
%%     end.
%% 
%% fire(Name, Employees) ->
%%     case multi_index:fetch_all(Name, ?BY_NAME, Employees) of
%%         [_Emp] -> {ok, multi_index:erase(Name, ?BY_NAME, Employees)};
%%         _ -> {error, "This employee does not work here"}
%%     end.
%% 
%% department_list(Job, Employees) ->
%%     multi_index:fetch_all(Job, ?BY_JOB, Employees).
%% </pre></code>


-module(multi_index).

-export([erase/3, fetch/3, fetch_all/3, from_list/2, insert/2, new/1,
        replace/3, size/1, to_list/2, try_insert/2]).

%% @type multi_index(). An opaque term representing a multi index.

%% @type key_function() = function((term()) -> term()). A function which, when
%% given a value, returns a corresponding key used to index that value.

%% @type multi_index_option() = {ordered_unique, key_function()} |
%% {ordered_non_unique, key_function()}.
%% <i>ordered_unique</i> - generates a unique index where values are ordered by
%% their keys, which are obtained by calling the key function on each value.
%%
%% <i>ordered_non_unique</i> - generates a non-unique index where values are
%% ordered by their keys, which are obtained by calling the key function on
%% each value.

-type key_function() :: fun((term()) -> term()).

-record(multi_index, {
        indices = [] :: [ordered_unique | ordered_non_unique],
        key_funs = [] :: [key_function()],
        key_val_stores = [] :: [gb_tree()]}).

-type multi_index() :: #multi_index{}.
-type multi_index_option() :: {ordered_unique, key_function()} |
    {ordered_non_unique, key_function()}.


%% @spec erase(Key::Key, IndexNum::IndexNum, MI::MI) -> multi_index()
%%  Key = term()
%%  IndexNum = integer()
%%  MI = multi_index()
%% @doc Erases all elements, if any, from the multi index <code>MI</code> whose
%% key for index number <code>IndexNum</code> matches <code>Key</code>. The
%% resulting multi index is returned.
-spec erase(term(), integer(), multi_index()) -> multi_index().
erase(K, N, MI) when is_record(MI, multi_index), is_integer(N), N > 0,
        N =< length(MI#multi_index.indices) ->
    Idx = lists:nth(N, MI#multi_index.indices),
    KVS = lists:nth(N, MI#multi_index.key_val_stores),
    case fetch_all(K, Idx, KVS) of
        [] -> MI;
        [V] -> MI#multi_index{
                key_val_stores = erase_one(V, MI#multi_index.indices,
                    MI#multi_index.key_funs, MI#multi_index.key_val_stores)};
        Vs -> MI#multi_index{
                key_val_stores = erase_all(
                    lists:foldl(fun sets:add_element/2, sets:new(), Vs),
                    MI#multi_index.indices, MI#multi_index.key_funs,
                    MI#multi_index.key_val_stores)}
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


%% @spec fetch(Key::Key, IndexNum::IndexNum, MI::MI) -> Value
%%  Key = term()
%%  IndexNum = integer()
%%  MI = multi_index()
%%  Value = term()
%% @doc Fetches and returns a single element from the multi index
%% <code>MI</code> whose key for index number <code>IndexNum</code> matches
%% <code>Key</code>.  Assumes that a matching value is present, crashes
%% otherwise. If this can happen, use {@link fetch_all/2} instead. If
%% <code>IndexNum</code> is a non-unique index and more than one matching value
%% is found, it is unspecified which one is returned.
-spec fetch(term(), integer(), multi_index()) -> term().
fetch(K, N, MI) when is_record(MI, multi_index), is_integer(N), N > 0,
        N =< length(MI#multi_index.indices) ->
    Idx = lists:nth(N, MI#multi_index.indices),
    KVS = lists:nth(N, MI#multi_index.key_val_stores),
    fetch_one(K, Idx, KVS).

fetch_one(K, ordered_unique, KVS) ->
    gb_trees:get(K, KVS);
fetch_one(K, ordered_non_unique, KVS) ->
    [V | _] = gb_trees:get(K, KVS),
    V.


%% @spec fetch_all(Key::Key, IndexNum::IndexNum, MI::MI) -> [Value]
%%  Key = term()
%%  IndexNum = integer()
%%  MI = multi_index()
%%  Value = term()
%% @doc Looks up and returns a possibly empty list of all values in the multi
%% index <code>MI</code> and whose key for index number <code>IndexNum</code>
%% matches <code>Key</code>. If <code>IndexNum</code> is a non-unique index and
%% more than one matching value is found, it is unspecified which order the
%% values are returned in.
-spec fetch_all(term(), integer(), multi_index()) -> [term()].
fetch_all(K, N, MI) when is_record(MI, multi_index), is_integer(N), N > 0,
        N =< length(MI#multi_index.indices) ->
    Idx = lists:nth(N, MI#multi_index.indices),
    KVS = lists:nth(N, MI#multi_index.key_val_stores),
    fetch_all_1(K, Idx, KVS).

fetch_all_1(K, ordered_unique, KVS) ->
    case gb_trees:lookup(K, KVS) of
        {value, V} -> [V];
        none -> []
    end;
fetch_all_1(K, ordered_non_unique, KVS) ->
    case gb_trees:lookup(K, KVS) of
        {value, Vs} -> Vs;
        none -> []
    end.


%% @spec from_list(Values::Values, Options::Options) -> multi_index()
%%  Values = [term()]
%%  Options = [multi_index_option()]
%% @doc Returns a new multi index whose elements are the elements in
%% <code>Values</code>, and with options <code>Options</code>. Assumes that no
%% value in <code>Values</code> contains a key conflict with any other value in
%% <code>Values</code>, crashes if this happens.  If a different behavior is
%% needed, <code>lists:foldl</code> should be used to build up a new multi
%% index instead.
%% @see new/1
-spec from_list([term()], [multi_index_option()]) -> multi_index().
from_list(Vs, Opts) when length(Opts) > 0 ->
    lists:foldl(fun insert/2, new(Opts), Vs).


%% @spec insert(Value::Value, MI1::MI1) -> MI2
%%  Value = term()
%%  MI1 = multi_index()
%%  MI2 = multi_index()
%% @doc Inserts the element <code>Value</code> into the multi index
%% <code>MI1</code> and returns the resulting multi index <code>MI2</code>.
%% Assumes that <code>Value</code> does not contain a key conflict with any
%% value in <code>MI1</code>, crashes otherwise. If this can happen, use {@link
%% try_insert/2} instead.
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


%% @spec new(Options::Options) -> multi_index()
%%  Options = [multi_index_option()]
%% @doc Returns an empty multi index with options <code>Options</code>.
%%
%% Options are used to control what indices the multi index is created with.
%% When passing indices, the first element in the list of indices is the first
%% index, the second element (if any) is the second index, and so on. There
%% must be at least one index given.
-spec new([multi_index_option()]) -> multi_index().
new(Opts) when length(Opts) > 0 ->
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


%% @spec replace(Value1::Value1, Value2::Value2, MI::MI) -> multi_index()
%%  Value1 = term()
%%  Value2 = term()
%%  MI = multi_index()
%% @doc Replaces all instances of <code>Value1</code> with <code>Value2</code>
%% in the multi index <code>MI</code> and returns the resulting multi index.
%% Assumes at least one <code>Value1</code> exists in <code>MI</code>, crashes
%% otherwise.
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


%% @spec size(MI::MI) -> integer()
%%  MI = multi_index()
%% @doc Returns the number of elements stored in the multi index
%% <code>MI</code>.
-spec size(multi_index()) -> integer().
size(MI) when is_record(MI, multi_index) ->
    [Idx | _] = MI#multi_index.indices,
    [KVS | _] = MI#multi_index.key_val_stores,
    size(Idx, KVS).

size(ordered_unique, KVS) ->
    gb_trees:size(KVS);
size(ordered_non_unique, KVS) ->
    lists:foldl(fun(L, A) -> A + length(L) end, 0, gb_trees:values(KVS)).


%% @spec to_list(IndexNum::IndexNum, MI::MI) -> [Value]
%%  IndexNum = integer()
%%  MI = multi_index()
%%  Value = term()
%% @doc Returns a list of all values in the multi index <code>MI</code>, in the
%% order of their keys for index number <code>IndexNum</code>. If
%% <code>IndexNum</code> is a non-unique index and multiple values with the
%% same key are found, it is unspecified which order the equal keyed values are
%% placed in the list relative to each other.
-spec to_list(integer(), multi_index()) -> [term()].
to_list(N, MI) when is_record(MI, multi_index), is_integer(N), N > 0,
        N =< length(MI#multi_index.indices) ->
    Idx = lists:nth(N, MI#multi_index.indices),
    KVS = lists:nth(N, MI#multi_index.key_val_stores),
    to_list_1(Idx, KVS).

to_list_1(ordered_unique, KVS) ->
    gb_trees:values(KVS);
to_list_1(ordered_non_unique, KVS) ->
    lists:flatmap(fun(X) -> X end, gb_trees:values(KVS)).


%% @spec try_insert(Value::Value, MI1::MI1) -> {ok, MI2} | error
%%  Value = term()
%%  MI1 = multi_index()
%%  MI2 = multi_index()
%% @doc Attempt to insert the element <code>Value</code> into the multi index
%% <code>MI1</code>. If the element can be inserted, multi index
%% <code>MI2</code> is created with that element inserted and <code>{ok,
%% MI2}</code> is returned. If the element cannot be inserted because it
%% violates a unique key constraint in <code>MI1</code>, then
%% <code>error</code> is returned.
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
