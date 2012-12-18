%%%
%%% wfd.erl
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

-module(wfd).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template{file = code:priv_dir(wfd) ++ "/templates/base.html"}.

title() -> "Home".

content() -> 
    [#panel{style = "text-align: center; margin-bottom: 1em;", body = #image{image = "/images/logo.png", style = ""}}]
    
    ++

    case wf:user() of
        undefined -> [
            #panel{data_fields = [{role, controlgroup}], body = [
                #link{url = "/login", text = "Login", mobile_target = true, mobile_dialog = true, data_fields = [{role, button}]},
                #link{url = "/register", text = "Register", mobile_target = true, data_fields = [{role, button}]}
            ]}
        ];

        _User ->
            AccountPanel = #panel{data_fields = [{role, controlgroup}], body = [
                #link{url = "/settings", text = "Account Settings", mobile_target = true, data_fields = [{role, button}]},
                #link{url = "/logout", text = "Logout", mobile_target = true, data_fields = [{role, button}]}
            ]},

            case wf:role(user) of
                true -> [
                    #panel{style = "margin-bottom: 1em", data_fields = [{role, controlgroup}], body = [
                        #link{url = "/dishes", mobile_target = true, data_fields = [{role, button}], body = [#image{image = "/images/placeholder.png", style = "vertical-align: middle; margin: 0 10px 0 -52px"}, "Dishes"]},
                        #link{url = "/menus", mobile_target = true, data_fields = [{role, button}], body = [#image{image = "/images/placeholder.png", style = "vertical-align: middle; margin: 0 10px 0 -52px"}, "Menus"]},
                        #link{url = "/go_shopping", mobile_target = true, data_fields = [{role, button}], body = [#image{image = "/images/placeholder.png", style = "vertical-align: middle; margin: 0 10px 0 -52px"}, "Go Shopping"]}
                    ]},
                    AccountPanel#panel{data_fields = [{mini, true} | AccountPanel#panel.data_fields]}
                ];

                false -> [
                    #panel{style = "margin-bottom: 1em", class = ["ui-body", "ui-body-e"], body = [#p{text = "You must validate your e-mail address before using this app."}]},
                    AccountPanel
                ]
            end
    end.
