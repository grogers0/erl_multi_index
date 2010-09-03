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

-module(employee_list).

-define(BY_NAME, 1).
-define(BY_JOB, 2).

-export([new/0, hire/3, fire/2, department_list/2]).

new() ->
    multi_index:new([{ordered_unique, fun(X) -> element(1, X) end},
            {ordered_non_unique, fun(X) -> element(2, X) end}]).

hire(Name, Job, Employees) ->
    case multi_index:try_insert({Name, Job}, Employees) of
        {ok, NewEmployees} -> {ok, NewEmployees};
        error -> {error, "This employee already works here"}
    end.

fire(Name, Employees) ->
    case multi_index:fetch_all(Name, multi_index:view(?BY_NAME, Employees)) of
        [Emp] -> {ok, multi_index:erase(Name,
                    multi_index:view(?BY_NAME, Employees))};
        _ -> {error, "This employee does not work here"}
    end.

department_list(Job, Employees) ->
    multi_index:fetch_all(Job, multi_index:view(?BY_JOB, Employees)).
