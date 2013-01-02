%%%
%%% wfd_dish.erl
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

-module(wfd_dish).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("wfd.hrl").

main() ->
    case wfd_common:protected_page([user]) of
        {ok, Page} ->
            % The user is authorized to use this module
            case wfd_dish_server:get_dish(wf:url_decode(wf:path_info()), wf:user()) of
                {ok, Dish} ->
                    wf:state(dish, Dish),
                    Page;
                {error, no_such_dish} ->
                    wfd_404:main()
            end;
        {_Status, Page} ->
            % The user is not authorized to use this module
            Page
    end.

title() -> (wf:state(dish))#wfd_dish.name.

header() -> #panel{data_fields = [{role, header}], body = [
    #h1{text = (wf:state(dish))#wfd_dish.name},
    #link{url = "#delete_dish_popup", text = "Delete", mobile_target = true, data_fields = [{icon, delete}, {rel, popup}, {"position-to", window}, {transition, pop}], class = "ui-btn-right"}
]}.

content() -> 
    Dish = wf:state(dish),

    %%
    %% The 'upload' element generates a button by default,
    %% which JQM then transforms into an enhanced button.
    %% I just want a simple link, but I don't want to rewrite
    %% the upload element just yet.  For now, use javascript
    %% to replace the enhanced button with links:
    %%
    wf:replace("form div.ui-btn", [
        #link{style = "position: absolute; top: 0px; left: 0px; z-index: 1; margin: 2px; padding-left: 0.75em", text = "Change"},
        #link{style = "position: absolute; top: 0px; right: 0px; z-index: 3; padding: 2px 0.75em 2px 0", text = "Remove", postback = remove_image}
    ]),
    wf:wire("$('form').trigger('create')"),

    %%
    %% Generate ingredients list
    %%
    IngredientsListItems = lists:foldr(fun(Ingredient, IngredientsList) ->
        IngredientName = element(1, Ingredient),
        ListItemId = wf:temp_id(),
        StatusId = wf:temp_id(),
        [#mobile_listitem{id = ListItemId, body = #panel{class = "box", body = [
            #span{body = #button{text = "Remove", data_fields = [{icon, delete}, {iconpos, notext}, {shadow, false}], postback = {ask_delete_ingredient, IngredientName, ListItemId}}},
            #span{style = "width: 100%; padding: 0 0.5em", body = [
                IngredientName,
                #panel{id = StatusId}
            ]},
            #span{body = #textbox{placeholder = "Amt", style = "width: 75px"}}
        ]}} | IngredientsList]
    end, [], lists:sort(fun(Ingredient1, Ingredient2) -> string:to_lower(element(1, Ingredient1)) =< string:to_lower(element(1, Ingredient2)) end, Dish#wfd_dish.ingredients)),

    [
        #panel{id = notification, html_id = "notification", body = #h4{class = ["ui-body", "ui-body-e"], text = "Saved"}},
        #panel{class = "ui-grid-a", body = [
            #panel{html_id = "dish_settings", class = "ui-block-a", body = [
                #fieldset{legend_text = "Type:", data_fields = [{role, controlgroup}, {type, horizontal}], actions = #event{type = change, postback = type}, body = [
                    #radio{id = type, name = "type", text = "Entrée", value = "entree", checked = Dish#wfd_dish.type == entree},
                    #radio{id = type, name = "type", text = "Side", value =  "side", checked = Dish#wfd_dish.type == side}
                ]},

                #label{for = "servings", text = "Servings:"},
                #range{id = servings, html_id = "servings", min = 1, max = 10, value = Dish#wfd_dish.servings, actions = #event{type = change, actions = #validate{attach_to = servings_status, validators = [
                    #is_integer{text = "Must be a positive number"},
                    #custom{text = "Must be a positive number", function = fun(_Tag, Value) -> list_to_integer(Value) > 0 end}
                ]}, postback = servings}},
                #panel{id = servings_status}
            ]},

            #panel{class = "ui-block-b", style = "padding-left: 1em", body = [
                #image{image = "/images/placeholder-thumb.png", style = "width: 100%"},
                #upload{show_button = false}
            ]}
        ]},

        #mobile_list{class = "ingredients", data_fields = [{dividertheme, a}], body = [
            #mobile_list_divider{body = #panel{class = "box", body = [#span{style = "width: 100%", text = "Ingredients"}, #span{body = #link{url = wf:f("/add/ingredient?dish=~s", [wf:path_info()]), text = "Add", mobile_target = true, mobile_dialog = true, data_fields = [{theme, a}, {role, button}, {icon, add}, {iconpos, notext}]}}]}} | IngredientsListItems
        ]}
    ].

footer() -> [
    #panel{html_id = "delete_dish_popup", data_fields = [{role, popup}], body = [
        #panel{data_fields = [{role, header}], body = [
            #h1{text = "Delete Dish?"}
        ]},

        #panel{data_fields = [{role, content}], body = [
            #h3{text = "Are you sure you want to delete this dish?", class = "ui-title"},
            #p{text = "This action cannot be undone."},
            #panel{style = "text-align: right", body = [
                #button{text = "No", data_fields = [{inline, true}, {rel, back}], actions = #event{type = click, actions = "$('#delete_dish_popup').popup('close')"}},
                #button{text = "Yes", postback = delete_dish, data_fields = [{theme, b}, {inline, true}]}
            ]}
        ]}
    ]},

    #panel{html_id = "delete_ingredient_popup", data_fields = [{role, popup}], body = [
        #panel{data_fields = [{role, header}], body = [
            #h1{text = "Delete Ingredient?"}
        ]},

        #panel{id = delete_ingredient_popup_content, data_fields = [{role, content}]}
    ]}
].

event(type) ->
    % XXX: this should check the inputs, but it doesn't really matter
    wfd_dish_server:set_type((wf:state(dish))#wfd_dish.name, wf:user(), list_to_atom(wf:q(type))),
    flash_notification();

event(servings) ->
    wfd_dish_server:set_servings((wf:state(dish))#wfd_dish.name, wf:user(), list_to_integer(wf:q(servings))),
    flash_notification();

event(delete_dish) ->
    wfd_dish_server:delete_dish((wf:state(dish))#wfd_dish.name, wf:user()),
    wf:wire("$.mobile.changePage('/dishes')");

event({ask_delete_ingredient, IngredientName, ListItemId}) ->
    wf:update(delete_ingredient_popup_content, [
        #p{text = wf:f("Are you sure you want to remove \"~s\" from the dish?", [IngredientName])},
        #panel{style = "text-align: right", body = [
            #button{text = "No", data_fields = [{inline, true}, {rel, back}], actions = #event{type = click, actions = "$('#delete_ingredient_popup').popup('close')"}},
            #button{text = "Yes", postback = {delete_ingredient, IngredientName, ListItemId}, data_fields = [{theme, b}, {inline, true}]}
        ]}
    ]),
    wf:wire("$('#delete_ingredient_popup').trigger('create').popup('open')");

event({delete_ingredient, _IngredientName, ListItemId}) ->
    wf:wire("$('#delete_ingredient_popup').popup('close')"),
    wf:remove(ListItemId),
    wf:wire("$('.ingredients').listview('refresh')"),
    flash_notification().

flash_notification() ->
    % meh, this isn't ideal, but it works
    wf:wire(notification, "$(obj('notification')).stop().css({'opacity': 1})"),
    wf:wire(notification, #show{}),
    wf:wire(notification, #fade{speed = 1000}).
