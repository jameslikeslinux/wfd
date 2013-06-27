%%%
%%% wfd_unit_server.erl
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

-module(wfd_unit_server).
-author("James Lee <jlee@thestaticvoid.com>").
-behaviour(gen_server).
-export([start_link/0, get_appropriate_conversions/1, get_appropriate_conversions/2, convert/2, convert/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("wfd.hrl").

-record(state, {unit_graph, contexts}).

%%
%% API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_appropriate_conversions(FromUnit) ->
    get_appropriate_conversions(FromUnit, "").

get_appropriate_conversions(FromUnit, Ingredient) ->
    gen_server:call(?MODULE, {get_appropriate_conversions, FromUnit, Ingredient}).

convert(Amount, ToUnit) ->
    convert(Amount, ToUnit, "").

convert(Amount, ToUnit, Ingredient) ->
    gen_server:call(?MODULE, {convert, Amount, ToUnit, Ingredient}).

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
    digraph:add_vertex(UnitGraph, pinch),
    digraph:add_vertex(UnitGraph, dash),

    % Conversions
    add_conversion(UnitGraph, pinch, tsp, 16),
    add_conversion(UnitGraph, dash, tsp, 8),
    add_conversion(UnitGraph, tsp, tbsp, 3),
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

    Contexts = [
        {"butter", tbsp, stick, 8},
        {"salt", pinch, g, 4},
        {"sugar", pinch, g, 6},
        {"water", g, ml, 1}
    ],

    {ok, #state{unit_graph = UnitGraph, contexts = Contexts}}.

handle_call({get_appropriate_conversions, FromUnit, Ingredient}, _From, State) ->
    Reply = do_in_context(fun() ->
        lists:sort(digraph_utils:reachable([FromUnit], State#state.unit_graph))
    end, State#state.unit_graph, State#state.contexts, Ingredient),

    {reply, Reply, State};

handle_call({convert, {Amount, FromUnit}, ToUnit, Ingredient}, _From, State) ->
    Reply = do_in_context(fun() ->
        case digraph:get_path(State#state.unit_graph, FromUnit, ToUnit) of
            false ->
                {error, invalid_conversion};

            Path ->
                {ok, {path_cost(State#state.unit_graph, Path, Amount), ToUnit}}
        end
    end, State#state.unit_graph, State#state.contexts, Ingredient),

    {reply, Reply, State};

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

remove_conversion(UnitGraph, FromUnit, To) ->
    digraph:del_path(UnitGraph, FromUnit, To),
    digraph:del_path(UnitGraph, To, FromUnit).

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

do_in_context(Function, UnitGraph, Contexts, Ingredient) ->
    % Add ingredient-specific conversions to the graph
    lists:foreach(fun({_Ingredient, From, To, AmtOfFromInTo}) ->
        add_conversion(UnitGraph, From, To, AmtOfFromInTo)
    end, proplists:lookup_all(string:to_lower(Ingredient), Contexts)),

    Reply = Function(),

    % Remove the ingredient-specific conversions from the graph
    lists:foreach(fun({_Ingredient, From, To, _AmtOfFromInTo}) ->
        remove_conversion(UnitGraph, From, To)
    end, proplists:lookup_all(string:to_lower(Ingredient), Contexts)),

    Reply.



%%
%% Tests
%%
get_appropriate_conversions_test_() -> [
    {"All volumes",
    ?_assertEqual([cup,dash,floz,gal,liter,ml,pinch,pint,quart,tbsp,tsp], wfd_unit_server:get_appropriate_conversions(tsp))},

    {"All weights",
    ?_assertEqual([g,kg,lbs,oz], wfd_unit_server:get_appropriate_conversions(oz))},

    {"Handles invalid unit",
    ?_assertEqual([foo], wfd_unit_server:get_appropriate_conversions(foo))},

    {"Can't find path from volume to weight without ingredient",
    ?_assertNot(lists:member(kg, wfd_unit_server:get_appropriate_conversions(ml)))},

    {"Can't find path from volume to weight with an invalid ingredient",
    ?_assertNot(lists:member(kg, wfd_unit_server:get_appropriate_conversions(ml, "Foo")))},

    {"Can find path from volume to weight with a known ingredient",
    ?_assert(lists:member(kg, wfd_unit_server:get_appropriate_conversions(ml, "water")))},

    {"The ingredient is case-insensitive",
    ?_assert(lists:member(kg, wfd_unit_server:get_appropriate_conversions(ml, "Water")))},

    {"Specifying an ingredient doesn't permanently alter the unit graph",
    ?_assertNot(lists:member(kg, wfd_unit_server:get_appropriate_conversions(ml)))}
].

convert_test_() -> [
    {"Volume",
    ?_assertEqual({ok, {128.0, floz}}, wfd_unit_server:convert({1, gal}, floz))},

    {"Weight",
    ?_assertEqual({ok, {35.274, oz}}, wfd_unit_server:convert({1, kg}, oz))},

    {"Handles impossible conversion",
    ?_assertEqual({error, invalid_conversion}, wfd_unit_server:convert({1, kg}, floz))},

    {"The impossible becomes possible when specifying a known ingredient",
    ?_assertEqual({ok, {33.814, floz}}, wfd_unit_server:convert({1, kg}, floz, "water"))},

    {"The ingredient is case-insensitive",
    ?_assertEqual({ok, {16.0, pinch}}, wfd_unit_server:convert({1, tsp}, pinch, "SaLt``"))},
    
    {"Specifying an ingredient doesn't permanently alter the unit graph",
    ?_assertEqual({error, invalid_conversion}, wfd_unit_server:convert({1, kg}, floz))}
].
