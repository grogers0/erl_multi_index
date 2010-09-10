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

-export([erase/3, fetch/3, fetch_all/3, from_list/2, indices/1, insert/2,
        new/1, replace/3, size/1, to_list/2, try_insert/2]).

-compile(nowarn_unused_function).

%% @type multi_index(). An opaque term representing a multi index.

%% @type key_function() = function((term()) -> term()). A function which, when
%% given a value, returns a corresponding key used to index that value.

%% @type index() = {ordered_unique, key_function()} |
%% {ordered_non_unique, key_function()}.
%% <i>ordered_unique</i> - generates a unique index where values are ordered by
%% their keys, which are obtained by calling the key function on each value.
%%
%% <i>ordered_non_unique</i> - generates a non-unique index where values are
%% ordered by their keys, which are obtained by calling the key function on
%% each value.

-type key_function() :: fun((term()) -> term()).
-type index() :: {ordered_unique, key_function()} |
    {ordered_non_unique, key_function()}.

-type ordered_index() :: nil |
    {l | e | r, ordered_index(), term(), ordered_index()}.

-record(multi_index, {
        n = 0 :: integer(),
        indices = [] :: [index()],
        key_val_stores = [] :: [ordered_index()]}).

-type multi_index() :: #multi_index{}.


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
    {_IdxType, IdxFun} = lists:nth(N, MI#multi_index.indices),
    KVS = lists:nth(N, MI#multi_index.key_val_stores),
    case ordered_fetch_list(K, IdxFun, KVS) of
        [] -> MI;
        Vs -> MI#multi_index{
                n = MI#multi_index.n - length(Vs),
                key_val_stores = erase_list(Vs, MI#multi_index.indices,
                    MI#multi_index.key_val_stores)}
    end.

erase_list(_, [], []) -> [];
erase_list(Vs, [{ordered_unique, KF} | Idxs], [KVS | KVSs]) ->
    [lists:foldl(fun(V, KVS2) -> ordered_erase_value_unique(V, KF, KVS2) end,
            KVS, Vs) | erase_list(Vs, Idxs, KVSs)];
erase_list(Vs, [{ordered_non_unique, KF} | Idxs], [KVS | KVSs]) ->
    [lists:foldl(fun(V, KVS2) -> ordered_erase_value_non_unique(V, KF, KVS2)
            end, KVS, Vs) | erase_list(Vs, Idxs, KVSs)].


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
    {_IdxType, IdxFun} = lists:nth(N, MI#multi_index.indices),
    KVS = lists:nth(N, MI#multi_index.key_val_stores),
    try ordered_fetch_or_throw(K, IdxFun, KVS)
    catch throw:badarg -> erlang:error(badarg, [K, N, MI])
    end.


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
    {_IdxType, IdxFun} = lists:nth(N, MI#multi_index.indices),
    KVS = lists:nth(N, MI#multi_index.key_val_stores),
    ordered_fetch_list(K, IdxFun, KVS).


%% @spec from_list(Values::Values, Indices::Indices) -> multi_index()
%%  Values = [term()]
%%  Indices = [index()]
%% @doc Returns a new multi index whose elements are the elements in
%% <code>Values</code>, and with indicse <code>Indices</code>. Assumes that no
%% value in <code>Values</code> contains a key conflict with any other value in
%% <code>Values</code>, crashes if this happens.  If a different behavior is
%% needed, <code>lists:foldl</code> should be used to build up a new multi
%% index instead.
%% @see new/1
-spec from_list([term()], [index()]) -> multi_index().
from_list(Vs, Indices) when length(Indices) > 0 ->
    try lists:foldl(fun insert/2, new(Indices), Vs)
    catch throw:badarg -> erlang:error(badarg, [Vs, Indices])
    end.


%% @spec insert(Value::Value, MI::MI) -> multi_index()
%%  Value = term()
%%  MI = multi_index()
%% @doc Inserts the element <code>Value</code> into the multi index
%% <code>MI</code> and returns the resulting multi index.  Assumes that
%% <code>Value</code> does not contain a key conflict with any value in
%% <code>MI</code>, crashes otherwise. If this can happen, use {@link
%% try_insert/2} instead.
-spec insert(term(), multi_index()) -> multi_index().
insert(Val, MI) ->
    try MI#multi_index{
            n = MI#multi_index.n + 1,
            key_val_stores = insert_one(Val, MI#multi_index.indices,
                MI#multi_index.key_val_stores)}
    catch throw:badarg -> erlang:error(badarg, [Val, MI])
    end.

insert_one(_V, [], []) -> [];
insert_one(V, [{ordered_unique, KF} | Idxs], [KVS | KVSs]) ->
    [ordered_insert_unique_or_throw(V, KF, KVS) |
        insert_one(V, Idxs, KVSs)];
insert_one(V, [{ordered_non_unique, KF} | Idxs], [KVS | KVSs]) ->
    [ordered_insert_non_unique(V, KF, KVS) |
        insert_one(V, Idxs, KVSs)].


%% @spec indices(MI::MI) -> [index()]
%%  MI = multi_index()
%% @doc Returns the indices used in the multi index <code>MI</code>.
-spec indices(multi_index()) -> [index()].
indices(MI) when is_record(MI, multi_index) ->
    MI#multi_index.indices.


%% @spec new(Indices::Indices) -> multi_index()
%%  Indices = [index()]
%% @doc Returns an empty multi index with indices <code>Indices</code>.
%%
%% When passing indices, the first element in the list of indices is the first
%% index, the second element (if any) is the second index, and so on. There
%% must be at least one index given.
-spec new([index()]) -> multi_index().
new(Indices) when length(Indices) > 0 ->
    MI = lists:foldl(fun add_index/2, #multi_index{}, Indices),
    #multi_index{
        indices = lists:reverse(MI#multi_index.indices),
        key_val_stores = lists:reverse(MI#multi_index.key_val_stores)}.

add_index({ordered_unique, KeyFun}, MI) when is_function(KeyFun) ->
    #multi_index{
        indices = [{ordered_unique, KeyFun} | MI#multi_index.indices],
        key_val_stores = [ordered_new() | MI#multi_index.key_val_stores]};
add_index({ordered_non_unique, KeyFun}, MI) when is_function(KeyFun) ->
    #multi_index{
        indices = [{ordered_non_unique, KeyFun} | MI#multi_index.indices],
        key_val_stores = [ordered_new() | MI#multi_index.key_val_stores]}.


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
    try MI#multi_index{key_val_stores = replace(V1, V2, MI#multi_index.indices,
                MI#multi_index.key_val_stores)}
    catch throw:badarg -> erlang:error(badarg, [V1, V2, MI])
    end.

replace(_V1, _V2, [], []) -> {ok, []};
replace(V1, V2, [{ordered_unique, KF} | Idxs], [KVS | KVSs]) ->
    [ordered_replace_value_unique_or_throw(V1, V2, KF, KVS) |
        replace(V1, V2, Idxs, KVSs)];
replace(V1, V2, [{ordered_non_unique, KF} | Idxs], [KVS | KVSs]) ->
    [ordered_replace_value_non_unique(V1, V2, KF, KVS) |
        replace(V1, V2, Idxs, KVSs)].


%% @spec size(MI::MI) -> integer()
%%  MI = multi_index()
%% @doc Returns the number of elements stored in the multi index
%% <code>MI</code>.
-spec size(multi_index()) -> integer().
size(MI) when is_record(MI, multi_index) ->
    MI#multi_index.n.


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
    KVS = lists:nth(N, MI#multi_index.key_val_stores),
    ordered_to_list(KVS).


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
    try {ok, MI#multi_index{
                n = MI#multi_index.n + 1,
                key_val_stores = insert_one(Val, MI#multi_index.indices,
                    MI#multi_index.key_val_stores)}}
    catch throw:badarg -> error
    end.


%% implementation of ordered indices using AVL trees

ordered_new() -> nil.

ordered_fetch_or_throw(_K, _KF, nil) ->
    throw(badarg);
ordered_fetch_or_throw(K, KF, {_, L, V, R}) ->
    KV = KF(V),
    if
        K < KV -> ordered_fetch_or_throw(K, KF, L);
        K > KV -> ordered_fetch_or_throw(K, KF, R);
        K == KV -> V
    end.

ordered_fetch_list(K, KF, T) ->
    ordered_fetch_list(K, KF, T, []).

ordered_fetch_list(_K, _KF, nil, A) ->
    A;
ordered_fetch_list(K, KF, {_, L, V, R}, A) ->
    KV = KF(V),
    if
        K < KV -> ordered_fetch_list(K, KF, L, A);
        K > KV -> ordered_fetch_list(K, KF, R, A);
        K == KV -> ordered_fetch_list(K, KF, L,
                [V | ordered_fetch_list(K, KF, R, A)])
    end.

ordered_insert_unique_or_throw(V, KF, T) ->
    ordered_insert_unique_or_throw(KF(V), V, KF, T).

ordered_insert_unique_or_throw(_KVin, Vin, _KF, nil) ->
    {e, nil, Vin, nil};
ordered_insert_unique_or_throw(KVin, Vin, KF, {B, L, V, R}) ->
    KV = KF(V),
    if
        KVin < KV ->
            Lnew = ordered_insert_unique_or_throw(KVin, Vin, KF, L),
            case insert_increased_height(L, Lnew) of
                true -> balance({leftmore_bal_factor(B), Lnew, V, R});
                false -> {B, Lnew, V, R}
            end;
        KVin > KV ->
            Rnew = ordered_insert_unique_or_throw(KVin, Vin, KF, R),
            case insert_increased_height(R, Rnew) of
                true -> balance({rightmore_bal_factor(B), L, V, Rnew});
                false -> {B, L, V, Rnew}
            end;
        KVin == KV ->
            throw(badarg)
    end.

ordered_insert_non_unique(V, KF, T) ->
    ordered_insert_non_unique(KF(V), V, KF, T).

ordered_insert_non_unique(_KVin, Vin, _KF, nil) ->
    {e, nil, Vin, nil};
ordered_insert_non_unique(KVin, Vin, KF, {B, L, V, R}) ->
    KV = KF(V),
    if
        KVin < KV orelse (KVin == KV andalso Vin < V) ->
            Lnew = ordered_insert_non_unique(KVin, Vin, KF, L),
            case insert_increased_height(L, Lnew) of
                true -> balance({leftmore_bal_factor(B), Lnew, V, R});
                false -> {B, Lnew, V, R}
            end;
        KVin > KV orelse (KVin == KV andalso Vin >= V) ->
            Rnew = ordered_insert_non_unique(KVin, Vin, KF, R),
            case insert_increased_height(R, Rnew) of
                true -> balance({rightmore_bal_factor(B), L, V, Rnew});
                false -> {B, L, V, Rnew}
            end
    end.

ordered_erase_value_unique(V, KF, T) ->
    ordered_erase_value_unique(KF(V), V, KF, T).

ordered_erase_value_unique(_KVin, _Vin, _KF, nil) ->
    nil;
ordered_erase_value_unique(KVin, Vin, KF, {B, L, V, R}) ->
    KV = KF(V),
    if
        KVin < KV orelse (KVin == KV andalso Vin < V) ->
            Lnew = ordered_erase_value_unique(KVin, Vin, KF, L),
            case erase_decreased_height(L, Lnew) of
                true -> balance({rightmore_bal_factor(B), Lnew, V, R});
                false -> {B, Lnew, V, R}
            end;
        KVin > KV orelse (KVin == KV andalso Vin > V) ->
            Rnew = ordered_erase_value_unique(KVin, Vin, KF, R),
            case erase_decreased_height(R, Rnew) of
                true -> balance({leftmore_bal_factor(B), L, V, Rnew});
                false -> {B, L, V, Rnew}
            end;
        Vin == V ->
            if
                L == nil -> R;
                R == nil -> L;
                true ->
                    {Vnew, Lnew} = erase_largest_node(L),
                    case erase_decreased_height(L, Lnew) of
                        true -> balance({rightmore_bal_factor(B),
                                    Lnew, Vnew, R});
                        false -> {B, Lnew, Vnew, R}
                    end
            end
    end.

ordered_erase_value_non_unique(V, KF, T) ->
    case ordered_erase_value_non_unique(KF(V), V, KF, T) of
        {more, Tnew} -> ordered_erase_value_non_unique(V, KF, Tnew);
        {none, Tnew} -> Tnew
    end.

% can only erase values until a rebalance occurs, then must propagate it the
% whole tree to erase another one.
ordered_erase_value_non_unique(_KVin, _Vin, _KF, nil) ->
    {none, nil};
ordered_erase_value_non_unique(KVin, Vin, KF, {B, L, V, R}) ->
    KV = KF(V),
    if
        KVin < KV orelse (KVin == KV andalso Vin < V) ->
            {More, Lnew} = ordered_erase_value_non_unique(KVin, Vin, KF, L),
            case erase_decreased_height(L, Lnew) of
                true -> {More, balance({rightmore_bal_factor(B), Lnew, V, R})};
                false -> {More, {B, Lnew, V, R}}
            end;
        KVin > KV orelse (KVin == KV andalso Vin > V) ->
            {More, Rnew} = ordered_erase_value_non_unique(KVin, Vin, KF, R),
            case erase_decreased_height(R, Rnew) of
                true -> {More, balance({leftmore_bal_factor(B), L, V, Rnew})};
                false -> {More, {B, L, V, Rnew}}
            end;
        Vin == V ->
            if
                L == nil -> {more, R};
                R == nil -> {more, L};
                true ->
                    {Vnew, Lnew} = erase_largest_node(L),
                    case erase_decreased_height(L, Lnew) of
                        true -> {more, balance({rightmore_bal_factor(B),
                                        Lnew, Vnew, R})};
                        false -> ordered_erase_value_non_unique(KVin, Vin, KF,
                                {B, Lnew, Vnew, R})
                    end
            end
    end.

% todo - see if this can be improved by only traversing the tree once and doing
% both insert and delete
ordered_replace_value_unique_or_throw(V1, V2, KF, T) ->
    ordered_insert_unique_or_throw(V2, KF, ordered_erase_value_unique(V1, KF, T)).

% todo - see if this can be improved by adding the duplicate values at once
ordered_replace_value_non_unique(V1, V2, KF, T) ->
    N = count_value(V1, KF, T),
    T1 = ordered_erase_value_non_unique(V1, KF, T),
    lists:foldl(fun(X, A) -> ordered_insert_non_unique(X, KF, A) end, T1,
        lists:duplicate(N, V2)).

count_value(V, KF, T) ->
    count_value(KF(V), V, KF, T, 0).

count_value(_KVin, _Vin, _KF, nil, A) ->
    A;
count_value(KVin, Vin, KF, {_, L, V, R}, A) ->
    KV = KF(V),
    if
        KVin < KV -> count_value(KVin, Vin, KF, L, A);
        KVin > KV -> count_value(KVin, Vin, KF, R, A);
        Vin == V -> count_value(KVin, Vin, KF, L,
                count_value(KVin, Vin, KF, R, A + 1));
        KVin == KV -> count_value(KVin, Vin, KF, L,
                count_value(KVin, Vin, KF, R, A))
    end.


erase_largest_node({_B, L, V, nil}) ->
    {V, L};
erase_largest_node({B, L, V, R}) ->
    {Vnode, Rnew} = erase_largest_node(R),
    case erase_decreased_height(R, Rnew) of
        true -> {Vnode, balance({leftmore_bal_factor(B), L, V, Rnew})};
        false -> {Vnode, {B, L, V, Rnew}}
    end.

insert_increased_height({e, _, _, _}, {l, _, _, _}) -> true;
insert_increased_height({e, _, _, _}, {r, _, _, _}) -> true;
insert_increased_height(nil, {_, _, _, _}) -> true;
insert_increased_height(_, _) -> false.

erase_decreased_height({l, _, _, _}, {e, _, _, _}) -> true;
erase_decreased_height({r, _, _, _}, {e, _, _, _}) -> true;
erase_decreased_height({_, _, _, _}, nil) -> true;
erase_decreased_height(_, _) -> false.

leftmore_bal_factor(l) -> ll;
leftmore_bal_factor(e) -> l;
leftmore_bal_factor(r) -> e.

rightmore_bal_factor(l) -> e;
rightmore_bal_factor(e) -> r;
rightmore_bal_factor(r) -> rr.

balance({ll, {l, LL, LV, LR}, V, R}) -> % left left
    {e, LL, LV, {e, LR, V, R}};
balance({ll, {r, LL, LV, {LRB, LRL, LRV, LRR}}, V, R}) -> % left right
    case LRB of
        l -> {e, {e, LL, LV, LRL}, LRV, {r, LRR, V, R}};
        e -> {e, {e, LL, LV, LRL}, LRV, {e, LRR, V, R}};
        r -> {e, {l, LL, LV, LRL}, LRV, {e, LRR, V, R}}
    end;
balance({rr, L, V, {r, RL, RV, RR}}) -> % right right
    {e, {e, L, V, RL}, RV, RR};
balance({rr, L, V, {l, {RLB, RLL, RLV, RLR}, RV, RR}}) -> % right left
    case RLB of
        l -> {e, {e, L, V, RLL}, RLV, {r, RLR, RV, RR}};
        e -> {e, {e, L, V, RLL}, RLV, {e, RLR, RV, RR}};
        r -> {e, {l, L, V, RLL}, RLV, {e, RLR, RV, RR}}
    end;
balance({ll, {e, LL, LV, LR}, V, R}) -> % left even (only during deletion)
    {r, LL, LV, {l, LR, V, R}};
balance({rr, L, V, {e, RL, RV, RR}}) -> % right even (only during deletion)
    {l, {r, L, V, RL}, RV, RR};
balance(T) -> % no balancing needed
    T.

ordered_check_balance(T) ->
    case check_balance_2(T) of
        {ok, _} -> true;
        error -> false
    end.

check_balance_2(nil) ->
    {ok, 0};
check_balance_2({l, L, _, R}) ->
    case {check_balance_2(L), check_balance_2(R)} of
        {{ok, LBal}, {ok, RBal}} ->
            if  LBal == RBal + 1 -> {ok, LBal + 1};
                true -> error
            end;
        {error, _} -> error;
        {_, error} -> error
    end;
check_balance_2({e, L, _, R}) ->
    case {check_balance_2(L), check_balance_2(R)} of
        {{ok, LBal}, {ok, RBal}} ->
            if  LBal == RBal -> {ok, LBal + 1};
                true -> error
            end;
        {error, _} -> error;
        {_, error} -> error
    end;
check_balance_2({r, L, _, R}) ->
    case {check_balance_2(L), check_balance_2(R)} of
        {{ok, LBal}, {ok, RBal}} ->
            if  LBal + 1 == RBal -> {ok, RBal + 1};
                true -> error
            end;
        {error, _} -> error;
        {_, error} -> error
    end;
check_balance_2(_) ->
    error.

ordered_to_list(T) ->
    ordered_to_list(T, []).

ordered_to_list(nil, A) ->
    A;
ordered_to_list({_, L, V, R}, A) ->
    ordered_to_list(L, [V | ordered_to_list(R, A)]).
