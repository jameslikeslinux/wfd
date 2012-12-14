%%%
%%% wfd_utils.erl
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

-module(wfd_utils).
-author("James Lee <jlee@thestaticvoid.com>").
-export([run_cmd/1, send_email/4, protect_page/0]).
-include_lib("nitrogen_core/include/wf.hrl").

run_cmd(Cmd) ->
    [Status|RevOutput] = lists:reverse(string:tokens(os:cmd(Cmd ++ "; echo $?"), "\n")),
    Output = lists:reverse(RevOutput),
    error_logger:info_msg("Ran command: ~p~nExit status: ~s~nOutput:~n~s", [Cmd, Status, [["    ", O, $\n] || O <- Output]]),
    {list_to_integer(Status), Output}.

send_email(ToName, ToEmail, Subject, Message) ->
    gen_smtp_client:send({"wfd@pkgblender.org", [ToEmail],
        "From: pkgblender <wfd@pkgblender.org>\r\n"
        "To: " ++  ToName ++ " <" ++ ToEmail ++ ">\r\n"
        "Subject: [pkgblender] " ++ Subject ++ "\r\n"
        "\r\n" ++
        Message}, [{relay, "localhost"}, {port, 25}]).

protect_page() ->
    case wf:user() of
        undefined ->
            case wf:session_default(login_dialog_raised, false) of
                false ->
                    wf:session(login_dialog_raised, true),
                    wf:wire(#script{script = "$('#login_dialog').click()"}),
                    #template{file = code:priv_dir(wfd) ++ "/templates/login.html"};

                true ->
                    wf:session(login_dialog_raised, false),
                    #template{file = code:priv_dir(wfd) ++ "/templates/unauthorized.html"}
            end;

        _User ->
            #template{file = code:priv_dir(wfd) ++ "/templates/base.html"}
    end.
