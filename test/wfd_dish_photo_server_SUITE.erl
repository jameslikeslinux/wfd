-module(wfd_dish_photo_server_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [
    {group, test_add_photo},
    {group, test_get_photo},
    {group, test_remove_photo}
].

groups() -> [
    {test_add_photo, [], [test_add_valid_photo, test_add_invalid_photo, test_add_valid_photo_without_convert_command]},
    {test_get_photo, [], [test_get_valid_photo, test_get_invalid_photo]},
    {test_remove_photo, [], [test_remove_valid_photo, test_remove_invalid_photo]}
].

init_per_suite(Config) ->
    wfd_app:install([node()]),
    wfd_app:start(),
    Config.

end_per_suite(_Config) ->
    wfd_app:stop(),
    wfd_app:uninstall([node()]),
    ok.

init_per_group(Group, Config) when Group =:= test_get_photo;
                                   Group =:= test_remove_photo ->
    {ok, Uuid} = add_valid_photo(),
    [{photo_uuid, Uuid} | Config];    

init_per_group(_Group, Config) ->
    Config.

end_per_group(Group, Config) when Group =:= test_get_photo;
                                  Group =:= test_remove_photo ->
    ok = wfd_dish_photo_server:remove_photo(?config(photo_uuid, Config));

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(test_add_valid_photo_without_convert_command, Config) ->
    Path = os:getenv("PATH"),
    os:putenv("PATH", ""),
    [{path, Path} | Config];

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(test_add_valid_photo_without_convert_command, Config) ->
    os:putenv("PATH", ?config(path, Config));

end_per_testcase(_TestCase, _Config) ->
    ok.


%%
%% Test Cases
%%
test_add_valid_photo(_Config) ->
    {ok, _Uuid} = add_valid_photo().

test_add_invalid_photo(_Config) ->
    {error, processing_failed} = wfd_dish_photo_server:add_photo(code:priv_dir(wfd) ++ "/templates/base.html").

test_add_valid_photo_without_convert_command(_Config) ->
    {error, processing_failed} = add_valid_photo().

test_get_valid_photo(Config) ->
    Uuid = ?config(photo_uuid, Config),
    {ok, ThumbPhotoData} = wfd_dish_photo_server:get_photo(Uuid, thumb),
    {ok, FullPhotoData} = wfd_dish_photo_server:get_photo(Uuid, full).

test_get_invalid_photo(_Config) ->
    {error, no_such_photo} = wfd_dish_photo_server:get_photo("foobar", thumb),
    {error, no_such_photo} = wfd_dish_photo_server:get_photo("foobar", full).

test_remove_valid_photo(Config) ->
    Uuid = ?config(photo_uuid, Config),
    ok = wfd_dish_photo_server:remove_photo(Uuid),
    {error, no_such_photo} = wfd_dish_photo_server:get_photo(Uuid, thumb),
    {error, no_such_photo} = wfd_dish_photo_server:get_photo(Uuid, full).

test_remove_invalid_photo(_Config) ->
    ok = wfd_dish_photo_server:remove_photo("foobar").
    

%%
%% Helper Functions
%%
add_valid_photo() ->
    wfd_dish_photo_server:add_photo(code:priv_dir(wfd) ++ "/images/placeholder.png").
