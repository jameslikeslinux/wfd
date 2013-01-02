%%%
%%% wfd_new_dish.erl
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

-module(wfd_new_dish).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("wfd.hrl").

main() ->
    {_Status, Page} = wfd_common:protected_page([user]),
    Page.

title() -> "New Dish".

header() -> #panel{data_fields = [{role, header}], body = [
    #h1{text = "New Dish"}
]}.

content() ->
    wf:wire(submit, dish_name, #validate{attach_to = dish_name_status, validators = [
        #is_required{text = "Required"},
        #max_length{text = "Name is too long", length = 50},
        #custom{text = "Dish already exists", function = fun(_Tag, Value) -> not wfd_dish_server:dish_exists(Value, wf:user()) end}
    ]}),
    ResetButton = #event{type = click, actions = "$(obj('submit')).attr('value', 'Create').button('refresh')"},
    [
        #mobile_list{inset = false, body = [
            #mobile_listitem{body = [
                #span{id = dish_name_status},
                #label{for = "dish_name", text = "Dish Name:"},
                #textbox{id = dish_name, html_id = "dish_name", next = submit, actions = ResetButton}
            ]},
            #mobile_listitem{body = [
                #button{id = submit, text = "Create", postback = submit, data_fields = [{theme, b}], actions = #event{type = click, actions = "$(obj('submit')).attr('value', 'Creating...').button('refresh')"}}
            ]}
        ]}
    ].

event(submit) ->
    DishName = wf:q(dish_name),
            
    case wfd_dish_server:new_dish(DishName, wf:user()) of
        ok -> 
            wf:wire(wf:f("$.mobile.changePage('/dish/~s')", [wf:url_encode(string:to_lower(DishName))]));
            
        {error, _} ->
            wf:wire("$(obj('submit')).attr('value', 'Dish Creation Failed').button('refresh')")
    end.
