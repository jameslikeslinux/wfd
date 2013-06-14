%%%
%%% wfd_dish_photo.erl
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

-module(wfd_dish_photo).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("wfd.hrl").

main() ->
    case wfd_common:protected_page([user]) of
        {ok, _Page} ->
            % The user is authorized to use this module
            PathInfo = wf:path_info(),
            PathInfoNoExt = string:substr(PathInfo, 1, string:len(PathInfo) - string:len(".jpg")),
            {Size, DishName} = case lists:reverse(string:tokens(PathInfoNoExt, "+")) of
                ["thumb"|Rest] ->
                    {thumb, wf:url_decode(string:join(lists:reverse(Rest), "+"))};
                _ ->
                    {full, wf:url_decode(PathInfoNoExt)}
            end,

            case wfd_dish_server:get_photo(DishName, wf:user(), Size) of
                {ok, PhotoUuid, Photo} ->
                    case wf:header(if_none_match) of
                        PhotoUuid ->
                            wf:status_code(304),
                            wf:header("ETag", PhotoUuid),
                            "";
                        _OldEtag ->
                            wf:content_type("image/jpeg"),
                            wf:header("ETag", PhotoUuid),
                            Photo
                    end;
                {error, no_photo} ->
                    case wf:header(if_none_match) of
                        "placeholder" ->
                            wf:status_code(304),
                            wf:header("ETag", "placeholder"),
                            "";
                        _OldEtag ->
                            wf:content_type("image/jpeg"),
                            wf:header("ETag", "placeholder"),
                            {ok, PlaceholderImage} = file:read_file(code:priv_dir(wfd) ++ "/images/placeholder-thumb.png"),
                            PlaceholderImage
                    end;
                {error, no_such_dish} ->
                    wfd_404:main()
            end;
        {_Status, Page} ->
            % The user is not authorized to use this module
            Page
    end.
