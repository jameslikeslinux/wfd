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

main() -> wfd_common:protected_page([user]).

title() -> "Spaghetti".

header() -> #panel{data_fields = [{role, header}], body = [
    #h1{text = "Spaghetti"},
    #button{text = "Edit", data_fields = [{icon, gear}], class = "ui-btn-right", actions = #event{type = click, actions = #script{script = ""}}}
]}.

content() -> [
    #panel{class = "ui-grid-a", body = [
        #panel{class = "ui-block-a", body = [
            #fieldset{legend_text = "Type:", data_fields = [{role, controlgroup}, {type, horizontal}], body = [
                #radio{name = "type", text = "Entrée", checked = true},
                #radio{name = "type", text = "Side"}
            ]},

            #label{for = "servings", text = "Servings:"},
            #textbox{id = servings, html_id = "servings", text = "4"}
        ]},

        #panel{class = "ui-block-b", style = "padding-left: 1em", body = [
            #image{image = "/images/placeholder-thumb.png", style = "width: 100%"}
        ]}
    ]},

    #mobile_list{class = "ingredients", data_fields = [{dividertheme, a}], body = [
        #mobile_list_divider{body = #panel{class = "box", body = [#span{style = "width: 100%", text = "Ingredients"}, #span{body = #button{text = "Add", data_fields = [{icon, add}, {iconpos, notext}]}}]}},
        #mobile_listitem{body = #panel{class = "box", body = [
            #span{body = #button{text = "Remove", data_fields = [{icon, delete}, {iconpos, notext}, {shadow, false}]}},
            #span{style = "width: 100%; padding: 0 0.5em", text = "Ingredient Name"},
            #span{body = #textbox{placeholder = "Amt", style = "width: 75px"}}
        ]}},
        #mobile_listitem{body = #panel{class = "box", body = [
            #span{body = #button{text = "Remove", data_fields = [{icon, delete}, {iconpos, notext}, {shadow, false}]}},
            #span{style = "width: 100%; padding: 0 0.5em", body = #textbox{id = test, placeholder = "New Ingredient", actions = [
                #event{type = change, actions = #validate{validators = [#custom{text = "Must be test", function = fun(_Tag, Value) -> Value == "test" end}]}, postback = test}
            ]}},
            #span{body = #textbox{placeholder = "Amt", style = "width: 75px"}}
        ]}}
    ]}
].

event(test) ->
    wf:wire(#script{script = wf:f("alert('got ~s')", [wf:q(test)])}).
