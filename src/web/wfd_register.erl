%%%
%%% wfd_register.erl
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

-module(wfd_register).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("wfd.hrl").

main() -> #template{file = code:priv_dir(wfd) ++ "/templates/base.html"}.

title() -> "Create Account".

header() -> #panel{data_fields = [{role, header}], body = [
    #h1{text = "Create Account"}
]}.

content() ->
    wf:wire(submit, username, #validate{attach_to = username_status, validators = [
        #is_required{text = "Required"},
        #min_length{text = "Must have at least 3 characters", length = 3},
        #max_length{text = "Maximum of 20 characters", length = 20},
        #custom{text = "Username already registered", function = fun user_not_exists/2}
    ]}),
    wf:wire(submit, password, #validate{attach_to = password_status, validators = [
        #is_required{text = "Required"},
        #min_length{text = "Must have at least 8 characters", length = 8},
        #custom{text = "Must have at least one lower-case character", function = fun matches/2, tag = "[a-z]"},
        #custom{text = "Must have at least one upper-case character", function = fun matches/2, tag = "[A-Z]"},
        #custom{text = "Must have at least one digit", function = fun matches/2, tag = "[0-9]"}
    ]}),
    wf:wire(submit, email, #validate{attach_to = email_status, validators = [
        #is_required{text = "Required"},
        #is_email{text = "Not a valid e-mail address"},
        #custom{text = "E-mail address already registered", function = fun email_not_registered/2}
    ]}),

    ResetButton = #event{type = click, actions = #script{script = "$(obj('submit')).attr('value', 'Register').button('refresh')"}},

    [
        #mobile_list{inset = false, body = [
            #mobile_listitem{body = [
                #span{id = username_status},
                #label{for = "username", body = ["Username:", #br{}, #span{class = "hint", text = "(3-20 characters)"}]},
                #textbox{id = username, html_id = "username", next = password, actions = ResetButton}
            ]},

            #mobile_listitem{body = [
                #span{id = password_status},
                #label{for = "password", body = ["Password:", #br{}, #span{class = "hint", text = "(≥ 8 characters, 1 a-z, 1 A-Z, 1 0-9)"}]},
                #password{id = password, html_id = "password", next = email, actions = ResetButton}
            ]},

            #mobile_listitem{body = [
                #span{id = email_status},
                #label{for = "email", text = "E-mail Address:"},
                #textbox{id = email, html_id = "email", next = submit, actions = ResetButton}
            ]},

            #mobile_listitem{body = [
                #button{id = submit, text = "Register", postback = register, data_fields = [{theme, b}], actions = #event{type = click, actions = #script{script = "$(obj('submit')).attr('value', 'Registering...').button('refresh')"}}}
            ]}
        ]},

        #link{id = success_dialog, url = "/register_success", mobile_target = true, mobile_dialog = true, style = "display: none;"}
    ].

event(register) ->
    [Username, Password, Email] = wf:mq([username, password, email]),
    case wfd_user_server:register_user(Username, Password, Email) of
        {ok, User} ->
            wfd_login:event(login),
            wfd_common:send_validation_email(User),
            wf:wire(#script{script = "$(obj('success_dialog')).click()"});
        error ->
            wf:wire(#script{script = "$(obj('submit')).attr('value', 'Registration Failed').button('refresh')"})
    end.

matches(Regex, String) ->
    case re:run(String, Regex) of
        {match, _} -> true;
        nomatch -> false
    end.

user_not_exists(_Tag, Username) ->
    not wfd_user_server:user_exists(Username).

email_not_registered(_Tag, Email) ->
    not wfd_user_server:email_registered(Email).
