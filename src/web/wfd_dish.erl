%%%
%%% wfd_dish.erl
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

-module(wfd_dish).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nitrogen_core/include/simple_bridge.hrl").
-include("wfd.hrl").

main() ->
    case wfd_common:protected_page([user]) of
        {ok, Page} ->
            % The user is authorized to use this module
            case wfd_dish_server:get_dish(wf:url_decode(wf:path_info()), wf:user()) of
                {ok, Dish} ->
                    wf:state(dish, Dish),

                    Req = wf_context:request_bridge(),
                    case Req:post_files() of
                        [UploadedFile|_Rest] ->
                            wf:info("Got uploaded file ~p~n", [UploadedFile]),
                            wf:session(uploaded_file, UploadedFile),
                            "";
                        _ ->
                            Page
                    end;
                {error, no_such_dish} ->
                    wfd_404:main()
            end;
        {_Status, Page} ->
            % The user is not authorized to use this module
            Page
    end.

%%
%% The jQuery Fileupload initialization must occur after the fileupload.js
%% stuff is loaded.  This is kind of a tricky exercise in getting code
%% to execute in the right order.  This function is called by the template
%% to ensure it is put in the right place.
%%
js() -> "$(function() {
    $('.restful_upload').fileupload({
        start: function(e) {
            $('#upload_buttons').hide();
            $('.progress-bar').show();
        },
        stop: function(e, data) {
            $('.progress-bar').hide();
            $('#progress_bar').val(0);
            $('#progress_bar').slider('refresh');
            $('#upload_buttons').show();
        },
        progressall: function(e, data) {
            var progress = parseInt(data.loaded / data.total * 100, 10);
            $('#progress_bar').val(progress);
            $('#progress_bar').slider('refresh');
        },
        done: function(e, data) {
            page.uploadComplete();
        }
    });
});".

title() -> (wf:state(dish))#wfd_dish.name.

header() -> #panel{data_fields = [{role, header}], body = [
    #h1{text = (wf:state(dish))#wfd_dish.name},
    #link{url = "#delete_dish_popup", text = "Delete", mobile_target = true, data_fields = [{icon, delete}, {rel, popup}, {"position-to", window}, {transition, ?test(none, pop)}], class = "ui-btn-right"}
]}.

content() -> 
    Dish = wf:state(dish),

    wf:wire(#api{name = uploadComplete}),

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
                #image{id = photo, image = "/dish/photo/" ++ wf:path_info() ++ "+thumb.jpg", style = "width: 100%"},
                #panel{id = progress, html_id = "progress", class = "progress-bar", style = "display: none", body = [
                    #range{html_id = "progress_bar", min = 0, max = 100, value = 0, data_fields = [{highlight, true}, {mini, true}]}
                ]},
                #panel{html_id = "upload_buttons", style = "text-align: center", data_fields = [{role, controlgroup}, {type, horizontal}, {mini, true}], body = [
                    #panel{class = "fileinput-button", data_fields = [{role, button}], body = [
                        #span{text = "Upload"},
                        #restful_upload{id = upload, data_fields = [{role, none}]}
                    ]},
                    #link{data_fields = [{role, button}], text = "Remove", postback = remove_photo}
                ]}
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
    ]},

    #panel{html_id = "photo_error_popup", data_fields = [{role, popup}], body = [
        #panel{data_fields = [{role, content}], body = [
            #h3{text = "Your image could not be processed.", class = "ui-title"},
            #p{text = "Maybe it was too large or in the wrong format."},
            #panel{style = "text-align: right", body = [
                #button{text = "Ok", data_fields = [{inline, true}, {rel, back}], actions = #event{type = click, actions = "$('#photo_error_popup').popup('close')"}}
            ]}
        ]}
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

event(remove_photo) ->
    wfd_dish_server:remove_photo((wf:state(dish))#wfd_dish.name, wf:user()),
    wf:wire(wf:f("$(obj('photo')).attr('src', '~s?' + new Date().getTime())", ["/dish/photo/" ++ wf:path_info() ++ "+thumb.jpg"]));

event({ask_delete_ingredient, IngredientName, ListItemId}) ->
    wf:update(delete_ingredient_popup_content, [
        #p{text = wf:f("Are you sure you want to remove \"~s\" from the dish?", [IngredientName])},
        #panel{style = "text-align: right", body = [
            #button{text = "No", data_fields = [{inline, true}, {rel, back}], actions = #event{type = click, actions = "$('#delete_ingredient_popup').popup('close')"}},
            #button{text = "Yes", postback = {delete_ingredient, IngredientName, ListItemId}, data_fields = [{theme, b}, {inline, true}]}
        ]}
    ]),
    wf:wire("$('#delete_ingredient_popup').trigger('create').popup('open')");

event({delete_ingredient, IngredientName, ListItemId}) ->
    wf:wire("$('#delete_ingredient_popup').popup('close')"),
    wfd_dish_server:remove_ingredient((wf:state(dish))#wfd_dish.name, wf:user(), IngredientName),
    wf:remove(ListItemId),
    wf:wire("$('.ingredients').listview('refresh')"),
    flash_notification().

flash_notification() ->
    % meh, this isn't ideal, but it works
    wf:wire(notification, "$(obj('notification')).stop().css({'opacity': 1})"),
    wf:wire(notification, #show{}),
    wf:wire(notification, #fade{speed = 1000}).


api_event(uploadComplete, _, _) ->
    UploadedFile = wf:session(uploaded_file),
    wf:info("Processing uploaded file ~p~n", [UploadedFile]),
    LocalFileName = UploadedFile#uploaded_file.temp_file,
    case wfd_dish_server:change_photo((wf:state(dish))#wfd_dish.name, wf:user(), LocalFileName) of
        ok ->
            wf:wire(wf:f("$(obj('photo')).attr('src', '~s?' + new Date().getTime())", ["/dish/photo/" ++ wf:path_info() ++ "+thumb.jpg"]));
        _Error ->
            wf:wire("$('#photo_error_popup').trigger('create').popup('open')")
    end,
    file:delete(LocalFileName).
