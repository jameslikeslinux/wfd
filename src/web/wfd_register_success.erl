%%%
%%% wfd_register_success.erl
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

-module(wfd_register_success).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template{file = code:priv_dir(wfd) ++ "/templates/base.html"}.

title() -> "Registration Successful".

content() -> [
    #p{text = "Your account was created successfully!  Before you can use this app, you must validate your e-mail address by clicking the link sent to your inbox."},
    #link{url = "/", text = "Continue", mobile_target = true, data_fields = [{role, button}]}
].
