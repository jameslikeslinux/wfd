%%%
%%% wfd_login.erl
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

-module(wfd_login).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("wfd.hrl").

main() -> #template{file = code:priv_dir(wfd) ++ "/templates/base.html"}.

title() -> "Login".

content() -> 
    ResetButton = #event{type = click, actions = #script{script = "$(obj('submit')).attr('value', 'Login').button('refresh')"}},
    [
        #p{id = blah},
        #label{for = "username", text = "Username:", class = "ui-hidden-accessible"},
        #textbox{id = username, html_id = "username", placeholder = "Username", next = password, actions = ResetButton},
        #label{for = "password", text = "Password:", class = "ui-hidden-accessible"},
        #password{id = password, html_id = "password", placeholder = "Password", next = submit, actions = ResetButton},
        #checkbox{id = remember_me, text = "Remember Me"},
        #button{id = submit, text = "Login", postback = login, actions = #event{type = click, actions = #script{script = "$(obj('submit')).attr('value', 'Checking...').button('refresh')"}}}
    ].

event(login) ->
    timer:sleep(3000),
    %wf:wire(#script{script = "$('.ui-dialog').dialog('close')"}),

    [Username, Password] = wf:mq([username, password]),


    RememberMe = case wf:q(remember_me) of
        "on" -> true;
        _ -> false
    end,

    % get the old remember-me cookie for removal from the database
    OldRememberMeSeries = case wf:cookie(remember_me_token) of
        "" -> "";
        Token -> hd(string:tokens(Token, ":"))
    end,

    case wfd_user_server:authenticate(Username, Password, RememberMe, OldRememberMeSeries) of
        {error, bad_auth} ->
            wf:wire(#script{script = "$(obj('submit')).attr('value', 'Invalid Username or Password').button('refresh')"});

        {ok, Roles, RememberMeToken} ->
            wf:user(Username),
            lists:foreach(fun(Role) -> wf:role(Role, true) end, Roles),
            case RememberMeToken of
                nil -> 
                    wf:cookie(remember_me_token, "", "/", 0);
                {Series, Value, _LastUsed} ->
                    wf:cookie(remember_me_token, Series ++ ":" ++ Value, "/", ?REMEMBER_ME_TTL)
            end,
            redirect_from_login("/")
    end.

%% modified from action_redirect.erl
%% required because requests are rewritten: / -> /wfd
%% XXX: this is a hack
%% remove "/wfd" from the URI
redirect_from_login(DefaultUrl) ->  
    PickledURI = wf:q(x),
    case wf:depickle(PickledURI) of
        undefined -> action_redirect:redirect(DefaultUrl);
        Other -> action_redirect:redirect(re:replace(Other, "/wfd", "", [{return, list}]))
    end.