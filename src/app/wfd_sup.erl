%%%
%%% wfd_sup.erl
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

-module(wfd_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%
%% API
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% Callbacks
%%
init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(wfd_dish_server, worker),
        ?CHILD(wfd_ingredient_server, worker),
        ?CHILD(wfd_unit_server, worker),
        ?CHILD(wfd_user_server, worker)
    ]}}.
