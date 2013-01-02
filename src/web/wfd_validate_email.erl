%%%
%%% wfd_validate_email.erl
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

-module(wfd_validate_email).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("wfd.hrl").

main() ->
    {_Status, Page} = wfd_common:protected_page([]),
    Page.

title() -> "E-mail Address Validation".

content() ->
    case wfd_user_server:validate_email(wf:user(), wf:q(token)) of
        {ok, User} ->
            lists:foreach(fun(Role) -> wf:role(Role, true) end, User#wfd_user.roles),
            [#h1{text = "Successfully Validated"},
             #p{text = "You have successfully validated your e-mail address. Thank you."}];
        {bad_validation_token, User} ->
            wfd_common:send_validation_email(User),
            [#h1{text = "Invalid Validation Token"},
             #p{text = "You have supplied an invalid e-mail validation token.  You have been sent a new validation e-mail.  Be sure to copy the link carefully."}];
        email_already_registered ->
            [#h1{text = "Address Already Registered"},
             #p{text = "Somebody else has already registered the same e-mail address.  Please change yours in the <a href='/settings'>account settings</a>."}];
        already_validated ->
            [#h1{text = "Already Validated"},
             #p{text = "You have already validated your e-mail address.  Thanks!"}]
    end ++ [#br{}, #link{url = "/", text = "Continue", mobile_target = true, data_fields = [{role, button}]}].
