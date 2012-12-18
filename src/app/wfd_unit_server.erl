%%%
%%% wfd_unit_server.erl
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

-module(wfd_unit_server).
-author("James Lee <jlee@thestaticvoid.com>").
-behaviour(gen_server).
-export([start_link/0, get_appropriate_conversions/1, convert/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("wfd.hrl").

-record(state, {unit_graph}).

%%
%% API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_appropriate_conversions(FromUnit) ->
    gen_server:call(?MODULE, {get_appropriate_conversions, FromUnit}).

convert(Amount, ToUnit) ->
    gen_server:call(?MODULE, {convert, Amount, ToUnit}).

%%
%% Callbacks
%%
init([]) ->
    UnitGraph = digraph:new(),

    % Volumes
    digraph:add_vertex(UnitGraph, tsp),
    digraph:add_vertex(UnitGraph, tbsp),
    digraph:add_vertex(UnitGraph, floz),
    digraph:add_vertex(UnitGraph, cup),
    digraph:add_vertex(UnitGraph, pint),
    digraph:add_vertex(UnitGraph, quart),
    digraph:add_vertex(UnitGraph, gal),
    digraph:add_vertex(UnitGraph, ml),
    digraph:add_vertex(UnitGraph, liter),

    % Weights
    digraph:add_vertex(UnitGraph, oz),
    digraph:add_vertex(UnitGraph, lbs),
    digraph:add_vertex(UnitGraph, g),
    digraph:add_vertex(UnitGraph, kg),

    % Misc
    digraph:add_vertex(UnitGraph, stick),

    % Conversions
    add_conversion(UnitGraph, tsp, tbsp, 3),
    add_conversion(UnitGraph, tbsp, stick, 8),
    add_conversion(UnitGraph, tbsp, floz, 2),
    add_conversion(UnitGraph, floz, ml, 0.033814),
    add_conversion(UnitGraph, ml, liter, 1000),
    add_conversion(UnitGraph, floz, cup, 8),
    add_conversion(UnitGraph, cup, pint, 2),
    add_conversion(UnitGraph, pint, quart, 2),
    add_conversion(UnitGraph, quart, gal, 4),
    add_conversion(UnitGraph, oz, lbs, 16),
    add_conversion(UnitGraph, oz, g, 0.035274),
    add_conversion(UnitGraph, g, kg, 1000),

    {ok, #state{unit_graph = UnitGraph}}.

handle_call({get_appropriate_conversions, FromUnit}, _From, State) ->
    {reply, digraph_utils:reachable([FromUnit], State#state.unit_graph), State};

handle_call({convert, {Amount, FromUnit}, ToUnit}, _From, State) ->
    UnitGraph = State#state.unit_graph,
    case digraph:get_path(UnitGraph, FromUnit, ToUnit) of
        false ->
            {reply, {error, invalid_conversion}, State};

        Path ->
            {reply, {ok, {path_cost(UnitGraph, Path, Amount), ToUnit}}, State}
    end;

handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%
%% Private Functions
%%
add_conversion(UnitGraph, FromUnit, To, AmtOfFromInTo) ->
    digraph:add_edge(UnitGraph, FromUnit, To, AmtOfFromInTo),
    digraph:add_edge(UnitGraph, To, FromUnit, 1 / AmtOfFromInTo).

path_cost(UnitGraph, [FromUnit, ToUnit], Cost) ->
    EdgeCost = find_edge_cost(UnitGraph, FromUnit, ToUnit),
    Cost / EdgeCost;
path_cost(UnitGraph, [FromUnit, ToUnit | Rest], Cost) ->
    EdgeCost = find_edge_cost(UnitGraph, FromUnit, ToUnit),
    path_cost(UnitGraph, [ToUnit | Rest], Cost / EdgeCost).

find_edge_cost(UnitGraph, FromUnit, ToUnit) ->
    Edges = digraph:out_edges(UnitGraph, FromUnit),
    lists:foldl(fun(Edge, Cost) ->
        case digraph:edge(UnitGraph, Edge) of
            {_E, FromUnit, ToUnit, Label} -> Label;
            _ -> Cost
        end
    end, 1, Edges).
