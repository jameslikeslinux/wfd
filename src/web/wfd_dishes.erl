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

main() -> wfd_common:protected_page([user]).

title() -> "Dishes".

header() -> #panel{data_fields = [{role, header}, {id, dishes_header}, {position, fixed}, {"tap-toggle", false}], body = [
    #link{url = "/dishes/new", text = "New Dish", mobile_target = true, data_fields = [{icon, plus}]},
    #h1{text = "Dishes"},
    #button{text = "Search", data_fields = [{icon, search}, {iconpos, notext}], class = "ui-btn-right", actions = #event{type = click, actions = #script{script = "$('.ui-listview-filter').show(); $('.ui-input-text').focus(); $.mobile.silentScroll(0)"}}}
]}.

content() -> [
    #mobile_list{id = dishes_list, html_id = "dishes_list", inset = false, data_fields = [{filter, true}], body = [
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h4{text = "Acura"}, #span{class = "ui-li-count", text = "$4.75"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Audi"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "BMW"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Cadillac"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Chrysler"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Dodge"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Ferrari"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Ford"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "GMC"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Honda"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Hyundai"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Infiniti"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Jeep"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Kia"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Lexus"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Mini"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Nissan"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Porsche"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Subaru"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Toyota"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Volkswagon"}]}]},
        #mobile_listitem{body = [#link{url = "/dish", data_fields = [{transition, slide}], mobile_target = true, body = [#image{image = "/images/placeholder-thumb.png"}, #h3{text = "Volvo"}]}]}
    ]}
].

footer() -> 
    {AllChecked, EntreesChecked, SidesChecked} = case wf:q(show) of
        "entrees" -> {false, true, false};
        "sides" -> {false, false, true};
         _ -> {true, false, false}
    end,

    #panel{data_fields = [{role, footer}, {id, dishes_footer}, {position, fixed}, {"tap-toggle", false}], body = [
        #panel{data_fields = [{role, controlgroup}, {type, horizontal}], style = "display: block; text-align: center;", body = [
            #radio{name = "view", text = "All", checked = AllChecked, actions = #event{type = click, actions = #script{script = "$.mobile.changePage('?show=all', {changeHash: false, transition: 'none'})"}}},
            #radio{name = "view", text = "Entrées", checked = EntreesChecked, actions = #event{type = click, actions = #script{script = "$.mobile.changePage('?show=entrees', {changeHash: false, transition: 'none'})"}}},
            #radio{name = "view", text = "Sides", checked = SidesChecked, actions = #event{type = click, actions = #script{script = "$.mobile.changePage('?show=sides', {changeHash: false, transition: 'none'})"}}}
        ]}
    ]}.
