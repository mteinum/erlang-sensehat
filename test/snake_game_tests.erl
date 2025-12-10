%%% @doc Unit tests for snake_game module
%%% @end

-module(snake_game_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Test wrap function (boundary wrapping for 8x8 grid)
wrap_negative_test() ->
    ?assertEqual(7, snake_game_test_helper:wrap(-1)),
    ?assertEqual(7, snake_game_test_helper:wrap(-2)).

wrap_positive_overflow_test() ->
    ?assertEqual(0, snake_game_test_helper:wrap(8)),
    ?assertEqual(0, snake_game_test_helper:wrap(9)).

wrap_valid_range_test() ->
    ?assertEqual(0, snake_game_test_helper:wrap(0)),
    ?assertEqual(3, snake_game_test_helper:wrap(3)),
    ?assertEqual(7, snake_game_test_helper:wrap(7)).

%% Test collision detection
check_collision_hit_test() ->
    Snake = [{3, 3}, {3, 4}, {3, 5}],
    % Head collides with body
    ?assert(snake_game_test_helper:check_collision({3, 4}, Snake)).

check_collision_miss_test() ->
    Snake = [{3, 3}, {3, 4}, {3, 5}],
    % Head doesn't collide
    ?assertNot(snake_game_test_helper:check_collision({4, 4}, Snake)).

check_collision_empty_test() ->
    ?assertNot(snake_game_test_helper:check_collision({3, 3}, [])).

%% Test movement
move_up_test() ->
    ?assertEqual({3, 2}, snake_game_test_helper:move({3, 3}, up)).

move_down_test() ->
    ?assertEqual({3, 4}, snake_game_test_helper:move({3, 3}, down)).

move_left_test() ->
    ?assertEqual({2, 3}, snake_game_test_helper:move({3, 3}, left)).

move_right_test() ->
    ?assertEqual({4, 3}, snake_game_test_helper:move({3, 3}, right)).

move_up_wrap_test() ->
    ?assertEqual({3, 7}, snake_game_test_helper:move({3, 0}, up)).

move_down_wrap_test() ->
    ?assertEqual({3, 0}, snake_game_test_helper:move({3, 7}, down)).

move_left_wrap_test() ->
    ?assertEqual({7, 3}, snake_game_test_helper:move({0, 3}, left)).

move_right_wrap_test() ->
    ?assertEqual({0, 3}, snake_game_test_helper:move({7, 3}, right)).
