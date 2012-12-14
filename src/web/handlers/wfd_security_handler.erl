%%%
%%% wfd_security_handler.erl
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

-module(wfd_security_handler).
-behaviour(security_handler).
-export([init/2, finish/2]).

-include("wfd.hrl").

init(_Config, State) -> 
    case wf:user() of
        undefined ->
            case string:tokens(wf:cookie(remember_me_token), ":") of
                [Series, Token] ->
                    case wfd_user_server:remember_me_login(Series, Token) of
                        {ok, User, Roles, {Series, NewToken, _LastUsed}} ->
                            wf:user(User),
                            lists:foreach(fun(Role) -> wf:role(Role, true) end, Roles),
                            wf:cookie(remember_me_token, Series ++ ":" ++ NewToken, "/", ?REMEMBER_ME_TTL);
                        {error, bad_series} ->
                            wf:cookie(remember_me_token, "", "/", 0);
                        {error, bad_token} ->  % theft assumed
                            wf:cookie(remember_me_token, "", "/", 0),
                            wf_context:page_module(wfd_cookie_theft)                                                       
                    end;
                _ ->
                    ok  % invalid cookie, but it's not worth erasing
            end; 
        _User ->
            ok  % the user is already logged in, continue on
    end,
    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.
