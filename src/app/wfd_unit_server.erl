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
-export([start_link/0, get_appropriate_conversions/1, get_appropriate_conversions/2, convert/2, convert/3, parse/1, to_string/1, get_graph/0]).
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

convert({Amount, Unit}, ToUnit) ->
    convert({Amount, Unit}, ToUnit, "").

convert({Amount, Unit}, ToUnit, Ingredient) ->
    gen_server:call(?MODULE, {convert, {Amount, Unit}, ToUnit, Ingredient}).

parse(String) ->
    gen_server:call(?MODULE, {parse, String}).

to_string({Amount, Unit}) ->
    gen_server:call(?MODULE, {to_string, {Amount, Unit}}).

get_graph() ->
    gen_server:call(?MODULE, {get_graph}).

%%
%% Callbacks
%%
init([]) ->
    UnitGraph = digraph:new(),

    % Volumes
    digraph:add_vertex(UnitGraph, tsp, add_plurals(["tsp", "t", "teaspoon"])),
    digraph:add_vertex(UnitGraph, tbsp, add_plurals(["tbsp", "T", "tablespoon"])),
    digraph:add_vertex(UnitGraph, floz, add_plurals(["fl oz", "fluid oz", "fluid ounce"])),
    digraph:add_vertex(UnitGraph, cup, add_plurals(["cup", "c", "C"])),
    digraph:add_vertex(UnitGraph, pint, add_plurals(["pt", "pint"])),
    digraph:add_vertex(UnitGraph, quart, add_plurals(["qt", "quart"])),
    digraph:add_vertex(UnitGraph, gal, add_plurals(["gal", "gallon"])),
    digraph:add_vertex(UnitGraph, ml, add_plurals(["ml", "mL", "milliliter", "millilitre"])),
    digraph:add_vertex(UnitGraph, liter, add_plurals(["L", "l", "liter", "litre"])),

    % Weights
    digraph:add_vertex(UnitGraph, oz, add_plurals(["oz", "ounce"])),
    digraph:add_vertex(UnitGraph, lb, add_plurals(["lb", "pound"])),
    digraph:add_vertex(UnitGraph, g, add_plurals(["g", "gram"])),
    digraph:add_vertex(UnitGraph, kg, add_plurals(["kg", "kilogram"])),

    % Misc
    digraph:add_vertex(UnitGraph, count, ["", "ct", "count"]),
    digraph:add_vertex(UnitGraph, box, add_plurals(["box"])),
    digraph:add_vertex(UnitGraph, stick, add_plurals(["stick"])),
    digraph:add_vertex(UnitGraph, pinch, add_plurals(["pinch", "pn"])),
    digraph:add_vertex(UnitGraph, dash, add_plurals(["dash"])),

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
    add_conversion(UnitGraph, oz, lb, 16),
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

handle_call({parse, String}, _From, State) ->
    Reply = case string_to_number(string:strip(String)) of 
        {error, _Reason} ->
            {error, no_amount};
        {Amount, Rest} ->
            case Amount =< 0 of
                true ->
                    {error, invalid_amount};
                false ->
                    case find_vertex_by_label(State#state.unit_graph, string:strip(Rest)) of
                        {error, _Reason} ->
                            {error, unknown_unit};
                        Vertex ->
                            {Amount, Vertex}
                    end
            end
    end,
    {reply, Reply, State};

handle_call({to_string, {Amount, Unit}}, _From, State) ->
    {_Vertex, [Label|_OtherLables]} = digraph:vertex(State#state.unit_graph, Unit),
    {reply, string:strip(lists:flatten(io_lib:format("~p ~s", [Amount, Label]))), State};

handle_call({get_graph}, _From, State) ->
    {reply, State#state.unit_graph, State};

handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%
%% Private Functions
%%
add_plurals(Labels) ->
    lists:foldl(fun(Label, LabelsWithPlurals) ->
        LabelsWithPlurals ++ [Label, Label ++ "s", Label ++ "es"]
    end, [], Labels).

add_conversion(UnitGraph, FromUnit, To, AmtOfFromInTo) ->
    digraph:add_edge(UnitGraph, FromUnit, To, AmtOfFromInTo),
    digraph:add_edge(UnitGraph, To, FromUnit, 1 / AmtOfFromInTo).

remove_conversion(UnitGraph, FromUnit, To) ->
    digraph:del_path(UnitGraph, FromUnit, To),
    digraph:del_path(UnitGraph, To, FromUnit).

path_cost(UnitGraph, [FromUnit, ToUnit], Cost) ->
    EdgeCost = find_edge_cost(UnitGraph, FromUnit, ToUnit),
    PathCost = Cost / EdgeCost,

    % Return an integer instead of a float if the result is whole
    case PathCost == trunc(PathCost) of
        true ->
            trunc(PathCost);
        false ->
            PathCost
    end;
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

string_to_number(String) ->
    case {string:to_float(String), string:to_integer(String)} of
        {{error, _Reason}, {error, _Reason}} ->
            {error, no_number};
        {{Float, Rest}, {error, _Reason}} ->
            {Float, Rest};
        {{error, _Reason}, {Integer, Rest}} ->
            {Integer, Rest};
        {{Float, Rest}, {_Integer, _Rest}} ->
            {Float, Rest}
    end.

find_vertex_by_label(UnitGraph, Label) ->
    lists:foldl(fun(Vertex, TheVertex) ->
        {Vertex, Labels} = digraph:vertex(UnitGraph, Vertex),
        case lists:member(Label, Labels) of
            true ->
                Vertex;
            false ->
                TheVertex
        end
    end, {error, vertex_not_found}, digraph:vertices(UnitGraph)).



%%
%% Tests
%%
get_appropriate_conversions_test_() -> [
    {"All volumes",
    ?_assertEqual([cup,dash,floz,gal,liter,ml,pinch,pint,quart,tbsp,tsp], wfd_unit_server:get_appropriate_conversions(tsp))},

    {"All weights",
    ?_assertEqual([g,kg,lb,oz], wfd_unit_server:get_appropriate_conversions(oz))},

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
    ?_assertEqual({ok, {128, floz}}, wfd_unit_server:convert({1, gal}, floz))},

    {"Weight",
    ?_assertEqual({ok, {35.274, oz}}, wfd_unit_server:convert({1, kg}, oz))},

    {"Handles impossible conversion",
    ?_assertEqual({error, invalid_conversion}, wfd_unit_server:convert({1, kg}, floz))},

    {"The impossible becomes possible when specifying a known ingredient",
    ?_assertEqual({ok, {33.814, floz}}, wfd_unit_server:convert({1, kg}, floz, "water"))},

    {"The ingredient is case-insensitive",
    ?_assertEqual({ok, {16, pinch}}, wfd_unit_server:convert({1, tsp}, pinch, "SaLt``"))},
    
    {"Specifying an ingredient doesn't permanently alter the unit graph",
    ?_assertEqual({error, invalid_conversion}, wfd_unit_server:convert({1, kg}, floz))}
].

parse_test_() ->
    UnitGraph = wfd_unit_server:get_graph(),
    AllLabels = lists:sort(lists:foldl(fun(Vertex, LabelsAcc) ->
        {Vertex, Labels} = digraph:vertex(UnitGraph, Vertex),
        LabelsAcc ++ Labels
    end, [], digraph:vertices(UnitGraph))),
    AllLabelsSet = lists:sort(sets:to_list(sets:from_list(AllLabels))),

    [
        {"All unit labels are unique",
        ?_assertEqual(AllLabels, AllLabelsSet)},

        {"Parses valid, expected amount/unit",
        ?_assertEqual({1, tsp}, wfd_unit_server:parse("1 tsp"))},

        {"Parses valid, expected amount/unit with full unit name",
        ?_assertEqual({1, tsp}, wfd_unit_server:parse("1 teaspoon"))},

        {"Parses valid, expected amount/unit with plural unit name",
        ?_assertEqual({2, tsp}, wfd_unit_server:parse("2 tsps"))},

        {"Parses valid, expected amount/unit with plural full unit name",
        ?_assertEqual({2, tsp}, wfd_unit_server:parse("2 teaspoons"))},

        {"Parses valid, expected amount/unit with special plural name",
        ?_assertEqual({2, box}, wfd_unit_server:parse("2 boxes"))},

        {"Handles weird spacing",
        ?_assertEqual({1, tsp}, wfd_unit_server:parse("  1tsp    "))},

        {"Handles floating point amounts",
        ?_assertEqual({1.5, tsp}, wfd_unit_server:parse("1.5 tsp"))},

        {"Fails to parse zero amounts",
        ?_assertEqual({error, invalid_amount}, wfd_unit_server:parse("0 tsp"))},

        {"Fails to parse negative amounts",
        ?_assertEqual({error, invalid_amount}, wfd_unit_server:parse("-1 tsp"))},

        {"Parses unitless amount",
        ?_assertEqual({1, count}, wfd_unit_server:parse("1"))},

        {"Fails to parse amountless unit",
        ?_assertEqual({error, no_amount}, wfd_unit_server:parse("tsp"))},

        {"Fails to parse unknown unit",
        ?_assertEqual({error, unknown_unit}, wfd_unit_server:parse("1 foo"))}
    ].

to_string_test_() -> [
    {"Converts amount/unit to string",
    ?_assertEqual("1 tsp", wfd_unit_server:to_string({1, tsp}))},

    {"Converts floating point amount/unit to string",
    ?_assertEqual("1.5 cup", wfd_unit_server:to_string({1.5, cup}))},

    {"Converts unitless amount to string",
    ?_assertEqual("4", wfd_unit_server:to_string({4, count}))}
].
