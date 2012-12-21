%% Mnesia tables
-record(wfd_user, {
    username,
    password_hash,
    email,
    validation_token = "",
    roles = [],
    remember_me_tokens = []
}).

-record(wfd_dish, {
    name,
    user,
    type = entree,      % entree | side
    servings = 4,
    ingredients = [],   % [{"name", {value, unit}}]
    photo = <<>>
}).

%% Constants
-define(REMEMBER_ME_TTL, 40320).  % four weeks
