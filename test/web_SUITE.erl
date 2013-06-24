%%%
%%% web_SUITE.erl
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

-module(web_SUITE).
-author("James Lee <jlee@thestaticvoid.com>").
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("wfd.hrl").

all() -> [
    test_all
].

init_per_suite(Config) ->
    wfd_app:start(),
   
    % Create some users to be used by Capybara 
    test_helpers:create_user("pendinguser", "password"),
    User1 = test_helpers:create_user("validuser1", "password"),
    test_helpers:validate_user(User1),
    User2 = test_helpers:create_user("validuser2", "password"),
    test_helpers:validate_user(User2),

    % Create dishes to be used by Capybara
    test_helpers:create_entree("Entree 1", "validuser1"),
    test_helpers:create_entree("Entree 2", "validuser1"),
    test_helpers:create_entree("validuser2 Entree", "validuser2"),
    test_helpers:create_side("Side 1", "validuser1"),
    test_helpers:create_side("Side 2", "validuser1"),

    Config.

end_per_suite(_Config) ->
    wfd_app:uninstall([node()|nodes()]).

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.


%%
%% Test Cases
%%
test_all(Config) ->
    0 = run_rspec(Config, "").

test_home(Config) ->
    0 = run_rspec(Config, "home_spec.rb").

test_register(Config) ->
    0 = run_rspec(Config, "register_spec.rb").

test_dishes(Config) ->
    0 = run_rspec(Config, "dishes_spec.rb").


%%
%% Helper Functions
%%
run_rspec(Config, SpecName) ->
    SpecDir = filename:join(?config(data_dir, Config), "spec"),
    Port = erlang:open_port({spawn, "rspec -I " ++ SpecDir ++ " " ++ filename:join(SpecDir, SpecName)}, [exit_status]),
    {ExitStatus, Output} = read_output(Port, []),
    ct:pal(Output),
    ExitStatus.

read_output(Port, Output) ->
    receive
        {Port, {data, Line}} ->
            read_output(Port, [Line | Output]);
        {Port, {exit_status, ExitStatus}} ->
            {ExitStatus, lists:reverse(Output)}
    end.
