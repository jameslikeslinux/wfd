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
    Actions = case wf:user() of
        undefined -> [
            #link{url = "/login", text = "Login", mobile_target = true, mobile_dialog = true, data_fields = [{role, button}]},
            #link{url = "/register", text = "Register", mobile_target = true, data_fields = [{role, button}]}
        ];

        _User -> [
            #link{url = "/settings", text = "Account Settings", mobile_target = true, data_fields = [{role, button}]},
            #link{url = "/logout", text = "Logout", mobile_target = true, data_fields = [{role, button}]}
        ]
    end,

    [
        #panel{style = "text-align: center; margin-bottom: 1em;", body = #image{image = "/images/logo.png", style = ""}},
        #panel{class = ["ui-body", "ui-body-e"], body = [#p{text="blah"}]},
        #panel{data_fields = [{role, controlgroup}], body = Actions}
    ].
