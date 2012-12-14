%%%
%%% wfd_cookie_theft.erl
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

-module(wfd_cookie_theft).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template{file = code:priv_dir(wfd) ++ "/templates/base.html"}.

title() -> "Bad Cookie".

content() -> [
    #h1{text = "Who stole the cookie from the cookie jar?"},
    #p{class = "notification", body = "You are attempting to login with an old identifying cookie.  This could mean that someone else has stolen your login cookie.  All of your sessions have been logged-out."}
].
