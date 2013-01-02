%%%
%%% wfd_dish_server.erl
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

-module(wfd_dish_server).
-author("James Lee <jlee@thestaticvoid.com>").
-behaviour(gen_server).
-export([start_link/0, new_dish/2, dish_exists/2, delete_dish/2, get_dishes/1, get_dish/2, set_type/3, set_servings/3, dish_has_ingredient/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("wfd.hrl").

%%
%% API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_dish(Name, User) ->
    gen_server:call(?MODULE, {new_dish, Name, User}).

dish_exists(Name, User) ->
    gen_server:call(?MODULE, {dish_exists, Name, User}).

delete_dish(Name, User) ->
    gen_server:call(?MODULE, {delete_dish, Name, User}).

get_dishes(User) ->
    gen_server:call(?MODULE, {get_dishes, User}).

get_dish(Name, Dish) ->
    gen_server:call(?MODULE, {get_dish, Name, Dish}).

set_type(Name, User, Type) ->
    gen_server:call(?MODULE, {set_type, Name, User, Type}).

set_servings(Name, User, Servings) ->
    gen_server:call(?MODULE, {set_servings, Name, User, Servings}).

dish_has_ingredient(Name, User, IngredientName) ->
    gen_server:call(?MODULE, {dish_has_ingredient, Name, User, IngredientName}).

%%
%% Callbacks
%%
init([]) ->
    ok = mnesia:wait_for_tables([wfd_dish], 5000),
    {ok, []}.

handle_call({new_dish, Name, User}, _From, State) ->
    Dish = #wfd_dish{name = Name, user = User},

    F = fun() ->
        case dish_exists_1(Name, User) of
            true ->
                mnesia:abort(dish_already_exists);
            false ->
                mnesia:write(Dish)
        end
    end,

    case mnesia:transaction(F) of
        {aborted, dish_already_exists} ->
            {reply, {error, dish_already_exists}, State};
        {atomic, ok} ->
            {reply, ok, State}
    end;

handle_call({dish_exists, Name, User}, _From, State) ->
    {reply, dish_exists_1(Name, User), State};

handle_call({delete_dish, Name, User}, _From, State) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        case get_dish_1(Name, User) of
            {ok, Dish} ->
                mnesia:delete_object(Dish);
            _ ->
                ok
        end
    end),
    {reply, ok, State};

handle_call({get_dishes, User}, _From, State) ->
    {atomic, Dishes} = mnesia:transaction(fun() ->
        qlc:e(qlc:q([D || D <- mnesia:table(wfd_dish),
                          D#wfd_dish.user == User]))
    end),
    {reply, Dishes, State};

handle_call({get_dish, Name, User}, _From, State) ->
    {reply, get_dish_1(Name, User), State};

handle_call({set_type, Name, User, Type}, _From, State) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        case get_dish_1(Name, User) of
            {ok, Dish} ->
                mnesia:delete_object(Dish),
                mnesia:write(Dish#wfd_dish{type = Type});
            _ ->
                ok
        end
    end),
    {reply, ok, State};

handle_call({set_servings, Name, User, Servings}, _From, State) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        case get_dish_1(Name, User) of
            {ok, Dish} ->
                mnesia:delete_object(Dish),
                mnesia:write(Dish#wfd_dish{servings = Servings});
            _ ->
                ok
        end
    end),
    {reply, ok, State};

handle_call({dish_has_ingredient, Name, User, IngredientName}, _From, State) ->
    {atomic, Status} = mnesia:transaction(fun() ->
        case get_dish_1(Name, User) of
            {ok, Dish} ->
                proplists:is_defined(IngredientName, Dish#wfd_dish.ingredients);
            _ ->
                false
        end                        
    end),
    {reply, Status, State};

handle_call(_Msg, _From, State) -> {noreply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%
%% Private Functions
%%
get_dish_1(Name, User) ->
    F = fun() ->
        qlc:e(qlc:q([D || D <- mnesia:table(wfd_dish),
                          string:to_lower(D#wfd_dish.name) == string:to_lower(Name),
                          D#wfd_dish.user == User]))
    end,

    case mnesia:transaction(F) of
        {atomic, []} -> {error, no_such_dish};
        {atomic, [Dish]} -> {ok, Dish}
    end.

dish_exists_1(Name, User) ->
    case get_dish_1(Name, User) of
        {ok, _Dish} ->
            true;
        {error, no_such_dish} ->
            false
    end.
