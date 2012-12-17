%%%
%%% wfd_settings.erl
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

-module(wfd_settings).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("wfd.hrl").

main() -> wfd_common:protected_page([]).

title() -> "Account Settings".

header() -> #panel{data_fields = [{role, header}], body = [
    #h1{text = "Account Settings"}
]}.

content() ->
    {ok, Email} = wfd_user_server:get_email(wf:user()),

    wf:wire(submit, email, #validate{validators = [
        #is_required{text = "Required"},
        #is_email{text = "Not a valid e-mail address"},
        #custom{text = "E-mail address already registered", function = fun email_not_registered/2}
    ]}),

    wf:wire(submit, new_password, #validate{validators = [
        #custom{text = "Must have at least 8 characters", function = fun matches/2, tag = "^(.{8,})?$"},
        #custom{text = "Must have at least one lower-case character", function = fun matches/2, tag = "^(.*[a-z]+.*)?$"},
        #custom{text = "Must have at least one upper-case character", function = fun matches/2, tag = "^(.*[A-Z]+.*)?$"},
        #custom{text = "Must have at least one digit", function = fun matches/2, tag = "^(.*[0-9]+.*)?$"}
    ]}),

    wf:wire(submit, current_password, #validate{validators = [
        #is_required{text = "Required"},
        #custom{text = "Invalid password", function = fun wfd_user_server:check_password/2, tag = wf:user()}
    ]}),

    ResetButton = #event{type = click, actions = #script{script = "$(obj('submit')).attr('value', 'Save').button('refresh')"}},

    [
        #label{for = "email", text = "E-mail Address:"},
        #textbox{id = email, html_id = "email", next = new_password, actions = ResetButton, text = Email},

        #label{for = "new_password", body = ["New Password:", #br{}, #span{class = "hint", text = "(≥ 8 characters, 1 a-z, 1 A-Z, 1 0-9)"}]},
        #password{id = new_password, html_id = "new_password", next = current_password, actions = ResetButton},

        #label{for = "current_password", body = "Current Password:"},
        #password{id = current_password, html_id = "current_password", next = submit, actions = ResetButton},

        #br{},
        #button{id = submit, text = "Save", postback = save, data_fields = [{theme, b}], actions = #event{type = click, actions = #script{script = "$(obj('submit')).attr('value', 'Saving...').button('refresh')"}}},

        #link{id = success_dialog, url = "/settings_success", mobile_target = true, mobile_dialog = true, style = "display: none;"}
    ].

event(save) ->
    [Email, NewPassword] = wf:mq([email, new_password]),
    update_email(Email),
    update_password(NewPassword),
    wf:wire(#script{script = "$(obj('success_dialog')).click()"}).

matches(Regex, String) ->
    case re:run(String, Regex) of
        {match, _} -> true;
        nomatch -> false
    end.

email_not_registered(_Tag, Email) ->
    {ok, CurrentEmail} = wfd_user_server:get_email(wf:user()),
    string:equal(Email, CurrentEmail) orelse not wfd_user_server:email_registered(Email).

update_email(Email) ->
    {ok, CurrentEmail} = wfd_user_server:get_email(wf:user()),
    case string:equal(Email, CurrentEmail) of
        false ->
            {ok, NewUser} = wfd_user_server:update_email(wf:user(), Email),
            wf:clear_roles(),
            lists:foreach(fun(Role) -> wf:role(Role, true) end, NewUser#wfd_user.roles),
            wfd_common:send_validation_email(NewUser);

        true ->
            ok
    end.

update_password(NewPassword) ->
    case NewPassword == "" of
        false ->
            wfd_user_server:update_password(wf:user(), NewPassword);

        true ->
            ok
    end.
