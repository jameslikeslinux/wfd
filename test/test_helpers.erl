%%%
%%% test_helpers.erl
%%% Copyright (C) 2013 James Lee
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

-module(test_helpers).
-author("James Lee <jlee@thestaticvoid.com>").
-compile(export_all).

-include("wfd.hrl").

create_user(Username, Password) ->
    {ok, User} = wfd_user_server:register_user(Username, Password, Username ++ "@example.com"),
    User.

validate_user(User) ->
    wfd_user_server:validate_email(User#wfd_user.username, User#wfd_user.validation_token).
