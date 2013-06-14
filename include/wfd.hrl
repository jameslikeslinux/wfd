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
    cost = unknown,
    photo = none        % none | uuid
}).

-record(wfd_dish_photo, {
    uuid,
    thumbnail,
    original
}).

-record(wfd_ingredient, {
    name,
    user,
    price = {unknown, unknown},
    can_buy_exact_amount = false,
    must_use_used_by = immediately  % | soon | whenever    
}).

%% Constants
-define(REMEMBER_ME_TTL, 40320).  % four weeks
