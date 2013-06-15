-module(wfd_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [
    test_wfd_dish_photo_server,
    test_wfd_unit_server
].

init_per_suite(Config) ->
    wfd_app:install([node()]),
    wfd_app:start(),
    Config.

end_per_suite(_Config) ->
    wfd_app:stop(),
    wfd_app:uninstall([node()]),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.


%%
%% Test Cases
%%
test_wfd_dish_photo_server(Config) ->
    {error, processing_failed} = wfd_dish_photo_server:add_photo(filename:join(?config(data_dir, Config), "invalid-photo.jpg")),
    {ok, Uuid} = wfd_dish_photo_server:add_photo(filename:join(?config(data_dir, Config), "valid-photo.jpg")),

    {ok, ThumbPhotoData} = wfd_dish_photo_server:get_photo(Uuid, thumb),
    {ok, FullPhotoData} = wfd_dish_photo_server:get_photo(Uuid, full),

    ThumbPhoto = filename:join(?config(priv_dir, Config), "thumb.jpg"),
    FullPhoto = filename:join(?config(priv_dir, Config), "full.jpg"),
    ok = file:write_file(ThumbPhoto, ThumbPhotoData),
    ok = file:write_file(FullPhoto, FullPhotoData),

    true = string:str(os:cmd("identify " ++ ThumbPhoto), "JPEG 240x240") > 0,
    true = string:str(os:cmd("identify " ++ FullPhoto), "JPEG 1000x667") > 0,

    ok = wfd_dish_photo_server:remove_photo(Uuid),
    {error, no_such_photo} = wfd_dish_photo_server:get_photo(Uuid, thumb),
    {error, no_such_photo} = wfd_dish_photo_server:get_photo(Uuid, full).

test_wfd_unit_server(_Config) ->
    ok = eunit:test(wfd_unit_server, [verbose]).
