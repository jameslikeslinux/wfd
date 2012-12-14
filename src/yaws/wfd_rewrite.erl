%%%
%%% wfd_rewrite.erl
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

-module(wfd_rewrite).
-export([arg_rewrite/1]).

-include_lib("yaws/include/yaws_api.hrl").

arg_rewrite(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    try yaws_api:url_decode_q_split(Path) of
        {DecPath, _Query} ->
            case DecPath == "/" orelse not filelib:is_file(Arg#arg.docroot ++ DecPath) of
                true ->
                    Arg#arg{req = Req#http_request{path = {abs_path, "/wfd" ++ Path}}};
                false ->
                    Arg
            end
    catch
        exit:_ ->
            Arg
    end.
