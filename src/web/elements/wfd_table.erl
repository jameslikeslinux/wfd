% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wfd_table).
-include_lib ("nitrogen_core/include/wf.hrl").
-include("wfd.hrl").
-compile(export_all).

reflect() -> record_info(fields, wfd_table).

render_element(Record) -> 

    Header = case Record#wfd_table.header of
      [] -> "";
      _ -> wf_tags:emit_tag(thead, Record#wfd_table.header, [])
    end,

    Footer = case Record#wfd_table.footer of
      [] -> "";
      _ -> wf_tags:emit_tag(tfoot, Record#wfd_table.footer, [])
    end,

    Body = wf_tags:emit_tag(tbody, Record#wfd_table.rows, []),
    Content = [Header, Footer, Body ],

    wf_tags:emit_tag( table, Content, [
        {class, [table, Record#wfd_table.class]},
        {style, Record#wfd_table.style}
    ]).
