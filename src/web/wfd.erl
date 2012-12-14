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

content() -> [
    #p{text = "Your theme was successfully downloaded. You can use this page as a reference for how to link it up!"},
    #pre{text =
        "<strong>&lt;link rel=&quot;stylesheet&quot; href=&quot;themes/wfd.min.css&quot; /&gt;</strong>\n"
        "&lt;link rel=&quot;stylesheet&quot; href=&quot;http://code.jquery.com/mobile/1.2.0/jquery.mobile.structure-1.2.0.min.css&quot; /&gt;\n"
        "&lt;script src=&quot;http://code.jquery.com/jquery-1.8.2.min.js&quot;&gt;&lt;/script&gt;\n"
        "&lt;script src=&quot;http://code.jquery.com/mobile/1.2.0/jquery.mobile-1.2.0.min.js&quot;&gt;&lt;/script&gt;", html_encode = false
    },
    #p{body = ["This is content color swatch \"A\" and a preview of a ", #link{url="/login", text = "link", mobile_target = true, mobile_dialog = true}, "."]},
    #label{for = "slider1", text = "Input slider:"},
    #range{html_id = "slider1", min = 0, max = 100, value = 50},
    #fieldset{data_fields = [{role, "controlgroup"}, {type, "horizontal"}], legend_text = "Cache settings:", body = [
        #radio{name = "radio-choice-a1", value = "on", checked = true, text = "On"},
        #radio{name = "radio-choice-a1", value = "off", text = "Off"}
    ]}
].
