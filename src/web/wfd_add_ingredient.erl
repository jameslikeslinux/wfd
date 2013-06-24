%%%
%%% wfd_add_ingredient.erl
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

-module(wfd_add_ingredient).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("wfd.hrl").

main() ->
    case wfd_common:protected_page([user]) of
        {ok, Page} ->
            % The user is authorized to use this module
            case wf:q(dish) of
                undefined ->
                    wfd_404:main();
                DishName ->
                    case wfd_dish_server:get_dish(wf:url_decode(DishName), wf:user()) of
                        {ok, Dish} ->
                            wf:state(dish, Dish),
                            Page;
                        {error, no_such_dish} ->
                            wfd_404:main()
                    end
            end;
        {_Status, Page} ->
            % The user is not authorized to use this module
            Page
    end.

title() -> "Add Ingredient".

header() -> #panel{data_fields = [{role, header}], body = [
    #h1{text = "Add Ingredient"}
]}.

content() ->
    wf:wire(submit, ingredient_name, #validate{attach_to = ingredient_name_status, validators = [
        #is_required{text = "Required"}
    ]}),
    
    ResetButton = #event{type = click, actions = #script{script = "$(obj('submit')).attr('value', 'Add').button('refresh')"}},
    
    %%
    %% Filtering and sorting are done strictly for presentation, so I am
    %% putting that logic here.  If it becomes a performance bottleneck,
    %% it can be pushed further down the stack to the data access layer.
    %%
    Ingredients = wfd_ingredient_server:get_ingredients(wf:user()),

    ListItems = lists:foldr(fun(Ingredient, IngredientsList) ->
        [#mobile_listitem{data_fields = [{icon, false}], body = #link{text = Ingredient#wfd_ingredient.name, mobile_target = true, actions = #event{type = click, actions = wf:f("$(obj('ingredient_name')).val('~s')", [wf:js_escape(Ingredient#wfd_ingredient.name)])}}} | IngredientsList]
    end, [], lists:sort(fun(Ingredient1, Ingredient2) -> string:to_lower(Ingredient1#wfd_ingredient.name) =< string:to_lower(Ingredient2#wfd_ingredient.name) end, Ingredients)),

    [
        #mobile_list{inset = false, body = [
            #mobile_listitem{body = [
                #span{id = ingredient_name_status},
                #label{for = "ingredient_name", text = "Type an ingredient name:"},
                #textbox{id = ingredient_name, html_id = "ingredient_name", actions = ResetButton},

                #label{for = "search-input", text = "or select from the list below:", class = "ui-input-text"},
                #panel{html_id = "ingredients_list", actions = ResetButton, body = [
                    #mobile_list{style = "max-height: 150px; overflow-y: auto", data_fields = [{filter, true}], body = ListItems}
                ]}
            ]},
            #mobile_listitem{body = [
                #button{id = submit, text = "Add", postback = add, data_fields = [{theme, b}], actions = #event{type = click, actions = #script{script = "$(obj('submit')).attr('value', 'Adding...').button('refresh')"}}}
            ]}
        ]}
    ].

event(add) ->
    wfd_dish_server:add_ingredient((wf:state(dish))#wfd_dish.name, wf:user(), wf:q(ingredient_name)),
    wf:wire("$('.ui-dialog').dialog('close')").
