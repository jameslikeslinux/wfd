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
-export([send_email/4]).

send_email(ToName, ToEmail, Subject, Message) ->
    gen_smtp_client:send({"jlee@thestaticvoid.org", [ToEmail],
        "From: What's for Dinner? <jlee@thestaticvoid.com>\r\n"
        "To: " ++  ToName ++ " <" ++ ToEmail ++ ">\r\n"
        "Subject: [What's for Dinner?] " ++ Subject ++ "\r\n"
        "\r\n" ++
        Message}, [{relay, "localhost"}, {port, 25}]).
