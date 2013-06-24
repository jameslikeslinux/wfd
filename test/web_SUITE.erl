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
    test_web
].

init_per_suite(Config) ->
    wfd_app:start(),
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
test_web(Config) ->
    SpecDir = filename:join(?config(data_dir, Config), "spec"),
    Port = erlang:open_port({spawn, "rspec --format documentation -I " ++ SpecDir ++ " " ++ SpecDir}, [exit_status]),
    0 = read_output(Port).

read_output(Port) ->
    receive
        {Port, {data, Output}} -> io:format("~s", [Output]), read_output(Port);
        {Port, {exit_status, ExitStatus}} -> ExitStatus
    end.
