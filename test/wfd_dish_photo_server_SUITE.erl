-module(wfd_dish_photo_server_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [
    test_add_valid_photo,
    test_add_invalid_photo
].

init_per_suite(Config) ->
    wfd_app:install([node()]),
    wfd_app:start(),
    Config.

end_per_suite(Config) ->
    wfd_app:stop(),
    wfd_app:uninstall([node()]),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%
%% Test Cases
%%
test_add_valid_photo(_Config) ->
    {ok, _Uuid} = wfd_dish_photo_server:add_photo(code:priv_dir(wfd) ++ "/images/placeholder-thumb.png").

test_add_invalid_photo(_Config) ->
    2 =:= 2.
