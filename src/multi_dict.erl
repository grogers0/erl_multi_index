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

-module(multi_dict).

% different signatures from dict: new, from_list, store
% no append, append_list, merge, update_counter
-export([erase/2, fetch/2, fetch_keys/1, filter/2, find/2, fold/3, from_list/2,
        from_view/1, is_key/2, map/2, new/1, size/1, store/2, to_list/1,
        update/3, update/4, view/1, view/2]).

-type index() :: integer() | [integer()].
-type name_or_index() :: atom() | index().

%% Internal representation of a multi_dict,
%%  names - maps a Name to an Index
%%  indices - maps an Index to a map of Keys to UniqueIds
%%  default - the Index to use if none is specified
%%  tuples - maps a UniqueId to a tuple value
-record(multi_dict, {
        names = dict:new() :: dict(),
        indices = dict:new() :: dict(),
        default = undefined :: 'undefined' | index(),
        tuples = dict:new() :: dict()}).

-type multi_dict() :: #multi_dict{}.

%% A view into a multi_dict that has a very similar interface to dict
%%  indexed - a map from Key to UniqueId
%%  tupes - a map from UniqueId to a tuple value
-record(multi_dict_view, {
        index :: index(),
        indexed :: dict(),
        dict :: multi_dict()}).

-type view() :: #multi_dict_view{}.

-type key() :: term() | [term()].

-type option() :: {'index', index()} | {'index', index(), atom()} |
    {'default_index', index()} | {'default_index', index(), atom()}.

%% Exported functions



-spec erase(key(), view()) -> multi_dict().
erase(Key, View) ->
    Dict = View#multi_dict_view.dict,
    case dict:find(Key, View#multi_dict_view.indexed) of
        {ok, Id} ->
            Value = dict:fetch(Id, Dict#multi_dict.tuples),
            Dict#multi_dict{
                indices = dict:map(fun(Index, KeyIdDict) ->
                            dict:erase(key_from_tuple(Index, Value),
                                KeyIdDict) end, Dict#multi_dict.indices),
                tuples = dict:erase(Id, Dict#multi_dict.tuples)};
        error -> Dict
    end.

-spec fetch(key(), view()) -> tuple().
fetch(Key, View) ->
    try Id = dict:fetch(Key, View#multi_dict_view.indexed),
        dict:fetch(Id, (View#multi_dict_view.dict)#multi_dict.tuples)
    catch error:badarg -> erlang:error(badarg, [Key, View])
    end.

-spec fetch_keys(view()) -> [key()].
fetch_keys(View) ->
    dict:fetch_keys(View#multi_dict_view.indexed).

-spec filter(fun((tuple()) -> boolean()), multi_dict() | view()) ->
            multi_dict().
filter(Pred, DictOrView) ->
    dict_fun_wrap(fun(Dict) -> filter_dict(Pred, Dict) end, DictOrView).

-spec find(key(), view()) ->
    {'ok', tuple()} | 'error'.
find(Key, View) ->
    case dict:find(Key, View#multi_dict_view.indexed) of
        {ok, Id} ->
            {ok, dict:find(Id, (View#multi_dict_view.dict)#multi_dict.tuples)};
        error -> error
    end.

-spec fold(fun((tuple(), term()) -> term()), term(), multi_dict() | view()) ->
            term().
fold(Fun, Acc, DictOrView) ->
    dict_fun_wrap(fun(Dict) -> fold_dict(Fun, Acc, Dict) end, DictOrView).

-spec from_list([tuple()], [option()]) -> multi_dict().
from_list(List, Opts) ->
    lists:foldl(fun(Value, Dict) -> store(Value, Dict) end, new(Opts), List).

-spec from_view(view()) -> multi_dict().
from_view(View) ->
    View#multi_dict_view.dict.

-spec is_key(key(), view()) -> boolean().
is_key(Key, View) ->
    dict:is_key(Key, View#multi_dict_view.indexed).

-spec map(fun((tuple()) -> tuple()), multi_dict() | view()) -> multi_dict().
map(Fun, DictOrView) ->
    dict_fun_wrap(fun(Dict) -> map_dict(Fun, Dict) end, DictOrView).

-spec new([option()]) -> multi_dict().
new(Opts) ->
    try lists:foldl(fun add_option/2, #multi_dict{}, Opts)
    catch error:badarg -> erlang:error(badarg, [Opts])
    end.

-spec size(multi_dict() | view()) -> integer().
size(DictOrView) ->
    dict_fun_wrap(fun size_dict/1, DictOrView).

-spec store(tuple(), multi_dict() | view()) -> multi_dict().
store(Value, DictOrView) ->
    dict_fun_wrap(fun(Dict) -> store_dict(Value, Dict) end, DictOrView).

-spec to_list(multi_dict() | view()) -> list().
to_list(DictOrView) ->
    dict_fun_wrap(fun to_list_dict/1, DictOrView).

-spec update(key(), fun((tuple()) -> tuple()), view()) -> multi_dict().
update(Key, Fun, View) ->
    Value1 = fetch(Key, View),
    Value2 = Fun(Value1),
    store(Value2, View#multi_dict_view.dict).

-spec update(key(), fun((tuple()) -> tuple()), tuple(), view()) -> multi_dict().
update(Key, Fun, Initial, View) ->
    Value1 = case find(Key, View) of
        {ok, Value} -> Value;
        error -> Initial
    end,
    Value2 = Fun(Value1),
    store(Value2, View#multi_dict_view.dict).

-spec view(multi_dict()) -> view().
view(Dict) ->
    try assert_default_not_undefined(Dict),
        view(Dict#multi_dict.default, Dict)
    catch error:badarg -> erlang:error(badarg, [Dict])
    end.

-spec view(name_or_index(), multi_dict()) -> view().
view(Name, Dict) when is_atom(Name) ->
    try view(dict:fetch(Name, Dict#multi_dict.names), Dict)
    catch error:badarg -> erlang:error(badarg, [Name, Dict])
    end;
view(Index, Dict) ->
    try #multi_dict_view{index = Index,
            indexed = dict:fetch(Index, Dict#multi_dict.indices),
            dict = Dict}
    catch error:badarg -> erlang:error(badarg, [Index, Dict])
    end.

%% Internal functions

add_option({index, Index}, Dict) ->
    Dict#multi_dict{
        indices = dict:store(Index, dict:new(), Dict#multi_dict.indices)};
add_option({index, Index, Name}, Dict) ->
    Dict#multi_dict{
        names = dict:store(Name, Index, Dict#multi_dict.names),
        indices = dict:store(Index, dict:new(), Dict#multi_dict.indices)};
add_option({default_index, Index}, Dict) ->
    assert_default_undefined(Dict),
    Dict#multi_dict{
        indices = dict:store(Index, dict:new(), Dict#multi_dict.indices),
        default = Index};
add_option({default_index, Index, Name}, Dict) ->
    assert_default_undefined(Dict),
    Dict#multi_dict{
        names = dict:store(Name, Index, Dict#multi_dict.names),
        indices = dict:store(Index, dict:new(), Dict#multi_dict.indices),
        default = Index};
add_option(_, _) ->
    erlang:error(badarg).

any_index(Dict) ->
    [Index | _] = dict:fetch_keys(Dict#multi_dict.indices),
    Index.

assert_default_not_undefined(Dict) ->
    if Dict#multi_dict.default /= undefined -> ok;
        true -> erlang:error(badarg)
    end.

assert_default_undefined(Dict) ->
    if Dict#multi_dict.default == undefined -> ok;
        true -> erlang:error(badarg)
    end.

dict_fun_wrap(Fun, Dict) when is_record(Dict, multi_dict) ->
    Fun(Dict);
dict_fun_wrap(Fun, View) when is_record(View, multi_dict_view) ->
    Fun(View#multi_dict_view.dict).

erase_all(Dict) ->
    Dict#multi_dict{
        indices = dict:map(fun(_K, _V) -> dict:new() end, Dict#multi_dict.indices),
        tuples = dict:new()}.

filter_dict(Pred, Dict) ->
    fold_dict(fun(Value, Acc) ->
                case Pred(Value) of
                    true -> Acc;
                    _ -> AnyIndex = any_index(Dict),
                        AnyView = view(AnyIndex, Dict),
                        erase(key_from_tuple(AnyIndex, Value), AnyView)
                end
        end, Dict, Dict).

fold_dict(Fun, Acc, Dict) ->
    dict:fold(fun(_K, V, A) -> Fun(V, A) end, Acc, Dict#multi_dict.tuples).

key_from_tuple(Index, Tuple) when is_integer(Index) ->
    element(Index, Tuple);
key_from_tuple(Indices, Tuple) when is_list(Indices) ->
    lists:map(fun(Index) -> element(Index, Tuple) end, Indices).

make_unique() ->
    {now(), self(), random:uniform()}.

map_dict(Fun, Dict) ->
    fold(fun(Value, Acc) -> store(Fun(Value), Acc) end, erase_all(Dict), Dict).

size_dict(Dict) ->
    dict:size(Dict#multi_dict.tuples).

store_dict(Value, InDict) ->
    AnyIndex = any_index(InDict),
    AnyView = view(AnyIndex, InDict),
    Dict = erase(key_from_tuple(AnyIndex, Value), AnyView),
    Id = make_unique(),
    Dict#multi_dict{
        indices = dict:map(fun(Index, KeyIdDict) ->
                    dict:store(key_from_tuple(Index, Value), Id, KeyIdDict) end,
            Dict#multi_dict.indices),
        tuples = dict:store(Id, Value, Dict#multi_dict.tuples)}.

to_list_dict(Dict) ->
    {_, List} = lists:unzip(dict:to_list(Dict#multi_dict.tuples)),
    List.
