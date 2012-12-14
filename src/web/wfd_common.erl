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

left_toolbar_links() -> [
    #link{text = "Dashboard", url = "#"},
    #link{text = "Packages", url = "#"},
    #link{text = "Builds", url = "#"},
    #link{text = "Upload", url = "/upload", show_if = wf:role(user)}
].

right_toolbar_links() ->
    case wf:user() of
        undefined -> [
            #link{text = "Login", url = "/login"},
            #link{text = "Register", url = "/register"}
        ];

        _User -> [
            #link{text = "Logout", url = "/logout"},
            #link{text = "Account Settings", url = "/account_settings"},
            #link{text = "Admin", url = "/admin", show_if = wf:role(admin)}
        ]
    end.

notification_area() ->
    case (wf:page_module() == wfd_validate_email) or
         (wf:user() == undefined) or
         (wf:role(user) or wf:session_default(notification_area_closed, false)) of
        true ->
            "";
        false ->
            % flash-like code from element_flash.erl
            #panel{id = notification_flash, style = "display: none;", class = "flash", actions = #show{effect = blind, speed = 400}, body = [
                #link{class = flash_close_button, text = "Close", postback = notification_area_closed, delegate = ?MODULE},
                #panel{id = flash_content, body = [
                    "You must validate your e-mail address before using pkgblender. ",
                    #link{text = "Resend validation e-mail...", postback = resend_validation_email, delegate = ?MODULE}
                ]}
            ]}
    end.

event(notification_area_closed) ->
    wf:session(notification_area_closed, true),
    wf:wire(notification_flash, #hide{effect = blind, speed = 400});

event(resend_validation_email) ->
    {bad_validation_token, User} = wfd_user_server:validate_email(wf:user(), "dummy-token"),
    send_validation_email(User),
    wf:replace(flash_content, #panel{id = flash_content, body = "A new validation e-mail has been sent to you."}).

send_validation_email(User) ->
    wfd_utils:send_email(User#wfd_user.name, User#wfd_user.email, "Validate Your E-mail Address",
        hd(string:tokens(User#wfd_user.name, " ")) ++ ",\n"
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
