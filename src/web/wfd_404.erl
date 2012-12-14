%%%
%%% wfd_404.erl
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

-module(wfd_404).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> 
    wf:status_code(404),
    #template{file = code:priv_dir(wfd) ++ "/templates/base.html"}.

title() -> "Page Not Found".

content() -> [
    #h1{text = "Page Not Found"},
    #p{class = "notification", body = "The page you requested does not exist."}
].
