%%% @doc Test helper for snake_game - exports private functions for testing
%%% @end

-module(snake_game_test_helper).

-export([
    wrap/1,
    move/2,
    check_collision/2
]).

-spec wrap(integer()) -> 0..7.
-spec move({0..7, 0..7}, up | down | left | right) -> {0..7, 0..7}.
-spec check_collision({0..7, 0..7}, [{0..7, 0..7}]) -> boolean().

%% Re-export private functions from snake_game for testing
wrap(N) when N < 0 -> 7;
wrap(N) when N > 7 -> 0;
wrap(N) when N >= 0, N =< 7 -> N.

move({X, Y}, up) -> {X, wrap(Y - 1)};
move({X, Y}, down) -> {X, wrap(Y + 1)};
move({X, Y}, left) -> {wrap(X - 1), Y};
move({X, Y}, right) -> {wrap(X + 1), Y}.

check_collision(Head, Snake) ->
    lists:member(Head, Snake).
