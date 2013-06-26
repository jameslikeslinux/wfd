%%%
%%% wfd_common.erl
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

-module(wfd_common).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("wfd.hrl").

send_validation_email(User) ->
    wfd_utils:send_email(User#wfd_user.username, User#wfd_user.email, "Validate Your E-mail Address",
        "Hi " ++ User#wfd_user.username ++ ",\n"
        "\n"
        "Before you can continue using pkgblender, you must validate your e-mail\n"
        "address by clicking the following link:\n"
        "\n"
        "    http://" ++ wf:header(host) ++ "/validate_email?token=" ++ User#wfd_user.validation_token ++ "\n"
        "\n"
        "If you did not register this e-mail address with the system, or you are\n"
        "having difficulty validating your address, please reply to this message\n"
        "and an administrator will help you.\n"
        "\n"
        "Thanks,\n"
        "\n"
        "pkgblender").

protected_page(Roles) ->
    case wf:user() of
        undefined ->
            case wf:session_default(login_dialog_raised, false) of
                false ->
                    wf:session(login_dialog_raised, true),
                    wf:wire(#script{script = "$('#login_dialog').click()"}),
                    {not_logged_in, #template{file = code:priv_dir(wfd) ++ "/templates/login.html"}};

                true ->
                    wf:session(login_dialog_raised, false),
                    {unauthorized, #template{file = code:priv_dir(wfd) ++ "/templates/unauthorized.html"}}
            end;

        _User ->
            case Roles == [] orelse lists:any(fun wf:role/1, Roles) of
                false ->
                    {unauthorized, #template{file = code:priv_dir(wfd) ++ "/templates/unauthorized.html"}};
                true ->
                    {ok, #template{file = code:priv_dir(wfd) ++ "/templates/base.html"}}
            end
    end.

js() ->
    case ?test of
        true ->
            "$(document).on('mobileinit', function() {"
            "    $.mobile.defaultDialogTransition = 'none';"
            "    $.mobile.defaultPageTransition = 'none';"
            "});";
        false ->
            ""
    end.
