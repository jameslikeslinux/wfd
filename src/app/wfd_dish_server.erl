%%%
%%% wfd_dish_server.erl
%%% Copyright (C) 2013 James Lee
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
-export([start_link/0, new_dish/2, dish_exists/2, delete_dish/2, get_dishes/1, get_dish/2, set_type/3, set_servings/3, change_photo/3, remove_photo/2, get_photo/3, add_ingredient/3, remove_ingredient/3]).
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

change_photo(Name, User, Filename) ->
    gen_server:call(?MODULE, {change_photo, Name, User, Filename}).

remove_photo(Name, User) ->
    gen_server:call(?MODULE, {remove_photo, Name, User}).

get_photo(Name, User, Size) ->
    gen_server:call(?MODULE, {get_photo, Name, User, Size}).

add_ingredient(Name, User, IngredientName) ->
    gen_server:call(?MODULE, {add_ingredient, Name, User, IngredientName}).

remove_ingredient(Name, User, IngredientName) ->
    gen_server:call(?MODULE, {remove_ingredient, Name, User, IngredientName}).

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
                wfd_dish_photo_server:remove_photo(Dish#wfd_dish.photo),
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

handle_call({change_photo, Name, User, Filename}, _From, State) ->
    Reply = case wfd_dish_photo_server:add_photo(Filename) of
        {ok, Uuid} ->
            {atomic, ok} = mnesia:transaction(fun() ->
                case get_dish_1(Name, User) of
                    {ok, Dish} ->
                        wfd_dish_photo_server:remove_photo(Dish#wfd_dish.photo),
                        mnesia:delete_object(Dish),
                        mnesia:write(Dish#wfd_dish{photo = Uuid});
                    _ ->
                        ok
                end
            end),
            ok;
        Error ->
            Error
    end,
    {reply, Reply, State};

handle_call({remove_photo, Name, User}, _From, State) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        case get_dish_1(Name, User) of
            {ok, Dish} ->
                case Dish#wfd_dish.photo of
                    none ->
                        ok;
                    Photo ->
                        wfd_dish_photo_server:remove_photo(Photo),
                        mnesia:delete_object(Dish),
                        mnesia:write(Dish#wfd_dish{photo = none})
                end;
            _ ->
                ok
        end
    end),
    {reply, ok, State};

handle_call({get_photo, Name, User, Size}, _From, State) ->
    {atomic, Reply} = mnesia:transaction(fun() ->
        case get_dish_1(Name, User) of
            {ok, Dish} ->
                case Dish#wfd_dish.photo of
                    none ->
                        {error, no_photo};
                    PhotoUuid ->
                        {ok, Photo} = wfd_dish_photo_server:get_photo(PhotoUuid, Size),
                        {ok, PhotoUuid, Photo}
                end;
            _Error ->
                {error, no_such_dish}
        end
    end),
    {reply, Reply, State};

handle_call({add_ingredient, Name, User, IngredientName}, _From, State) ->
    {atomic, Reply} = mnesia:transaction(fun() ->
        case get_dish_1(Name, User) of
            {ok, Dish} ->
                case proplists:is_defined(IngredientName, Dish#wfd_dish.ingredients) of
                    false ->
                        wfd_ingredient_server:new_ingredient(IngredientName, User),
                        mnesia:delete_object(Dish),
                        mnesia:write(Dish#wfd_dish{ingredients = [{IngredientName, unknown} | Dish#wfd_dish.ingredients]});
                    true -> 
                        {error, already_has_ingredient}
                end;
            _ ->
                {error, no_such_dish}
        end                        
    end),
    {reply, Reply, State};

handle_call({remove_ingredient, Name, User, IngredientName}, _From, State) ->
    {atomic, Reply} = mnesia:transaction(fun() ->
        case get_dish_1(Name, User) of
            {ok, Dish} ->
                mnesia:delete_object(Dish),
                mnesia:write(Dish#wfd_dish{ingredients = proplists:delete(IngredientName, Dish#wfd_dish.ingredients)}),
                case qlc:e(qlc:q([D || D <- mnesia:table(wfd_dish),
                                       proplists:is_defined(IngredientName, D#wfd_dish.ingredients)])) of
                    [] ->
                        wfd_ingredient_server:delete_ingredient(IngredientName, User);
                    _ ->
                        ok
                end;
            _ ->
                {error, no_such_dish}
        end                        
    end),
    {reply, Reply, State};

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
