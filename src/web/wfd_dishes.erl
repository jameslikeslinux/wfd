%%%
%%% wfd_dishes.erl
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

-module(wfd_dishes).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("wfd.hrl").

main() ->
    {_Status, Page} = wfd_common:protected_page([user]),
    Page.

title() -> "Dishes".

header() -> #panel{data_fields = [{role, header}, {id, dishes_header}, {position, fixed}, {"tap-toggle", false}], body = [
    #link{url = "/new/dish", text = "New Dish", mobile_target = true, mobile_dialog = true, data_fields = [{icon, plus}]},
    #h1{text = "Dishes"},
    #button{text = "Search", data_fields = [{icon, search}, {iconpos, notext}], class = "ui-btn-right", actions = #event{type = click, actions = #script{script = "$('.ui-listview-filter').show(); $('.ui-input-text').focus(); $.mobile.silentScroll(0)"}}}
]}.

content() ->
    %%
    %% Filtering and sorting are done strictly for presentation, so I am
    %% putting that logic here.  If it becomes a performance bottleneck,
    %% it can be pushed further down the stack to the data access layer.
    %%
    Dishes = wfd_dish_server:get_dishes(wf:user()),

    FilteredDishes = case wf:q(show) of
        "entrees" ->
            lists:filter(fun(Dish) -> Dish#wfd_dish.type == entree end, Dishes);
        "sides" ->
            lists:filter(fun(Dish) -> Dish#wfd_dish.type == side end, Dishes);
         _ ->
            Dishes
    end,

    ListItems = lists:foldr(fun(Dish, DishesList) ->
        DishUrlPath = wf:url_encode(string:to_lower(Dish#wfd_dish.name)),
        [#mobile_listitem{body = #link{url = "/dish/" ++ DishUrlPath, data_fields = [{transition, ?test(none, slide)}], mobile_target = true, body = [#image{image = "/dish/photo/" ++ DishUrlPath ++ "+thumb.jpg"}, #h4{text = Dish#wfd_dish.name}]}} | DishesList]
    end, [], lists:sort(fun(Dish1, Dish2) -> string:to_lower(Dish1#wfd_dish.name) =< string:to_lower(Dish2#wfd_dish.name) end, FilteredDishes)),

    #panel{html_id = "dishes_list", body = #mobile_list{inset = false, data_fields = [{filter, true}], body = ListItems}}.

footer() -> 
    {AllChecked, EntreesChecked, SidesChecked} = case wf:q(show) of
        "entrees" -> {false, true, false};
        "sides" -> {false, false, true};
         _ -> {true, false, false}
    end,

    #panel{data_fields = [{role, footer}, {id, dishes_footer}, {position, fixed}, {"tap-toggle", false}], body = [
        #panel{data_fields = [{role, controlgroup}, {type, horizontal}, {mini, true}], style = "display: block; text-align: center;", body = [
            #radio{name = "view", text = "All", checked = AllChecked, actions = #event{type = click, actions = #script{script = "$.mobile.changePage('?show=all', {changeHash: false, transition: 'none'})"}}},
            #radio{name = "view", text = "Entrées", checked = EntreesChecked, actions = #event{type = click, actions = #script{script = "$.mobile.changePage('?show=entrees', {changeHash: false, transition: 'none'})"}}},
            #radio{name = "view", text = "Sides", checked = SidesChecked, actions = #event{type = click, actions = #script{script = "$.mobile.changePage('?show=sides', {changeHash: false, transition: 'none'})"}}}
        ]}
    ]}.
