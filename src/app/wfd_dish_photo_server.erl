%%%
%%% wfd_dish_photo_server.erl
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

-module(wfd_dish_photo_server).
-author("James Lee <jlee@thestaticvoid.com>").
-behaviour(gen_server).
-export([start_link/0, add_photo/1, remove_photo/1, get_photo/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("wfd.hrl").

%%
%% API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_photo(Filename) ->
    gen_server:call(?MODULE, {add_photo, Filename}).

remove_photo(Uuid) ->
    gen_server:call(?MODULE, {remove_photo, Uuid}).

get_photo(Uuid, Size) ->
    gen_server:call(?MODULE, {get_photo, Uuid, Size}).

%%
%% Callbacks
%%
init([]) ->
    Frags = mnesia:activity(sync_dirty, fun() -> mnesia:table_info(wfd_dish_photo, frag_names) end, [], mnesia_frag),
    ok = mnesia:wait_for_tables(Frags, 5000),
    {ok, []}.

handle_call({add_photo, Filename}, _From, State) ->
    Reply = case process_image(Filename) of
        {ok, Thumbnail, Image} -> 
            Uuid = uuid:to_string(uuid:uuid4()),
            ok = mnesia:activity(transaction, fun() ->
                mnesia:write(#wfd_dish_photo{uuid = Uuid, thumbnail = Thumbnail, original = Image})
            end, [], mnesia_frag),
            {ok, Uuid};
        Error ->
            Error
    end,
    {reply, Reply, State};

handle_call({remove_photo, Uuid}, _From, State) ->
    ok = mnesia:activity(transaction, fun() ->
        mnesia:delete({wfd_dish_photo, Uuid})
    end, [], mnesia_frag),
    {reply, ok, State};

handle_call({get_photo, Uuid, Size}, _From, State) ->
    Reply = case mnesia:activity(transaction, fun() ->
                mnesia:read({wfd_dish_photo, Uuid})
            end, [], mnesia_frag) of
        [Photo] ->
            case Size of
                thumb ->
                    {ok, Photo#wfd_dish_photo.thumbnail};
                _OtherSize ->
                    {ok, Photo#wfd_dish_photo.original}
            end;
        _ ->
            {error, no_such_photo}
    end,
    {reply, Reply, State};    

handle_call(_Msg, _From, State) -> {noreply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%
%% Private Functions
%%
process_image(Filename) ->
    error_logger:info_msg("Processing ~s~n", [Filename]),
    case {
        convert_cmd(Filename ++ " -resize 240x240^ -gravity center -extent 240x240 -format jpg -"),
        convert_cmd(Filename ++ " -resize 1000x1000\\> -format jpg -")
    } of
        {{ok, Thumbnail}, {ok, Image}} -> {ok, Thumbnail, Image};
        _ -> {error, processing_failed}
    end.

convert_cmd(Cmd) ->
    case os:find_executable("convert") of
        false ->
            error_logger:error_msg("'convert' not found"),
            {error, convert_not_found};
        Convert ->
            Port = erlang:open_port({spawn, Convert ++ " " ++ Cmd}, [exit_status, binary]),
            read_convert_output(Port, [])
    end.

read_convert_output(Port, Data) ->
    receive
        {Port, {data, NewData}} -> read_convert_output(Port, [NewData|Data]);
        {Port, {exit_status, 0}} -> {ok, lists:reverse(Data)};
        {Port, {exit_status, _S}} -> {error, convert_failed}
    end.
