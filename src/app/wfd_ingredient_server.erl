%%%
%%% wfd_ingredient_server.erl
%%% Copyright (C) 2012 James Lee
%%% 
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%

-module(wfd_ingredient_server).
-author("James Lee <jlee@thestaticvoid.com>").
-behaviour(gen_server).
-export([start_link/0, new_ingredient/2, ingredient_exists/2, delete_ingredient/2, get_ingredients/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("wfd.hrl").

%%
%% API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_ingredient(Name, User) ->
    gen_server:call(?MODULE, {new_ingredient, Name, User}).

ingredient_exists(Name, User) ->
    gen_server:call(?MODULE, {ingredient_exists, Name, User}).

delete_ingredient(Name, User) ->
    gen_server:call(?MODULE, {delete_ingredient, Name, User}).

get_ingredients(User) ->
    gen_server:call(?MODULE, {get_ingredients, User}).

%%
%% Callbacks
%%
init([]) ->
    ok = mnesia:wait_for_tables([wfd_ingredient], 5000),
    {ok, []}.

handle_call({new_ingredient, Name, User}, _From, State) ->
    Ingredient = #wfd_ingredient{name = Name, user = User},

    F = fun() ->
        case ingredient_exists_1(Name, User) of
            true ->
                mnesia:abort(ingredient_already_exists);
            false ->
                mnesia:write(Ingredient)
        end
    end,

    case mnesia:transaction(F) of
        {aborted, ingredient_already_exists} ->
            {reply, {error, ingredient_already_exists}, State};
        {atomic, ok} ->
            {reply, ok, State}
    end;

handle_call({ingredient_exists, Name, User}, _From, State) ->
    {reply, ingredient_exists_1(Name, User), State};

handle_call({delete_ingredient, Name, User}, _From, State) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        case get_ingredient_1(Name, User) of
            {ok, Ingredient} ->
                mnesia:delete_object(Ingredient);
            _ ->
                ok
        end
    end),
    {reply, ok, State};

handle_call({get_ingredients, User}, _From, State) ->
    {atomic, Ingredientes} = mnesia:transaction(fun() ->
        qlc:e(qlc:q([D || D <- mnesia:table(wfd_ingredient),
                          D#wfd_ingredient.user == User]))
    end),
    {reply, Ingredientes, State};

handle_call(_Msg, _From, State) -> {noreply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%
%% Private Functions
%%
get_ingredient_1(Name, User) ->
    F = fun() ->
        qlc:e(qlc:q([D || D <- mnesia:table(wfd_ingredient),
                          string:to_lower(D#wfd_ingredient.name) == string:to_lower(Name),
                          D#wfd_ingredient.user == User]))
    end,

    case mnesia:transaction(F) of
        {atomic, []} -> {error, no_such_ingredient};
        {atomic, [Ingredient]} -> {ok, Ingredient}
    end.

ingredient_exists_1(Name, User) ->
    case get_ingredient_1(Name, User) of
        {ok, _Ingredient} ->
            true;
        {error, no_such_ingredient} ->
            false
    end.
