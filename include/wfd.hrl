%% Nitrogen elements
-include_lib("nitrogen_core/include/wf.hrl").
-record(wfd_table, {?ELEMENT_BASE(wfd_table), rows, header = [], footer = []}).

%% Mnesia tables
-record(wfd_user, {username, password_hash, email, validation_token = "", roles = [], remember_me_tokens = []}).

%% Constants
-define(REMEMBER_ME_TTL, 40320).  % four weeks
