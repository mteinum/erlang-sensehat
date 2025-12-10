%%% @doc Snake Game for Raspberry Pi Sense Hat
%%% A simple snake game controlled by the joystick.
%%% Use the joystick to control the snake direction.
%%% @end

-module(snake_game).
-author("Morten Teinum <morten.teinum@gmail.com>").
-export([start/0, stop/0]).

-define(GREEN, 16#00ff00).
-define(RED, 16#ff0000).
-define(BLACK, 16#000000).
-define(TICK_INTERVAL, 300).

-record(state, {
    snake :: [{pos_integer(), pos_integer()}],
    direction :: up | down | left | right,
    food :: {pos_integer(), pos_integer()},
    game_over :: boolean()
}).

-spec start() -> ok.
start() ->
    case whereis(snake_game) of
        undefined ->
            _ = sensehat:start(),
            _ = st_supervisor:start_link(),
            HandlerId = st_event_manager:subscribe(),
            Pid = spawn(fun() -> init(HandlerId) end),
            register(snake_game, Pid),
            ok;
        _Pid ->
            {error, already_running}
    end.

-spec stop() -> ok.
stop() ->
    case whereis(snake_game) of
        undefined -> ok;
        Pid ->
            Pid ! stop,
            unregister(snake_game),
            ok
    end.

-spec init(term()) -> ok.
init(HandlerId) ->
    InitialSnake = [{4, 4}, {4, 5}, {4, 6}],
    Food = spawn_food(InitialSnake),
    State = #state{
        snake = InitialSnake,
        direction = right,
        food = Food,
        game_over = false
    },
    _ = sensehat:clear(?BLACK),
    _ = draw(State),
    _ = timer:send_interval(?TICK_INTERVAL, self(), tick),
    loop(State, HandlerId).

-spec loop(#state{}, term()) -> ok.
loop(#state{game_over = true} = State, HandlerId) ->
    receive
        {st_event, enter} ->
            _ = st_event_manager:unsubscribe(HandlerId),
            init(HandlerId);
        stop ->
            _ = sensehat:clear(?BLACK),
            _ = st_event_manager:unsubscribe(HandlerId),
            ok;
        _ ->
            loop(State, HandlerId)
    after 100 ->
        flash_game_over(),
        loop(State, HandlerId)
    end;

loop(State, HandlerId) ->
    receive
        {st_event, Direction} when Direction =:= up; Direction =:= down;
                                    Direction =:= left; Direction =:= right ->
            NewState = change_direction(State, Direction),
            loop(NewState, HandlerId);
        
        {st_event, enter} ->
            loop(State, HandlerId);
        
        tick ->
            NewState = update_game(State),
            _ = draw(NewState),
            loop(NewState, HandlerId);
        
        stop ->
            _ = sensehat:clear(?BLACK),
            _ = st_event_manager:unsubscribe(HandlerId),
            ok
    after 100 ->
        loop(State, HandlerId)
    end.

-spec change_direction(#state{}, up | down | left | right) -> #state{}.
change_direction(#state{direction = up} = State, down) -> State;
change_direction(#state{direction = down} = State, up) -> State;
change_direction(#state{direction = left} = State, right) -> State;
change_direction(#state{direction = right} = State, left) -> State;
change_direction(State, NewDirection) ->
    State#state{direction = NewDirection}.

-spec update_game(#state{}) -> #state{}.
update_game(#state{snake = Snake, direction = Dir, food = Food} = State) ->
    [Head | _] = Snake,
    NewHead = move(Head, Dir),
    
    case check_collision(NewHead, Snake) of
        true ->
            State#state{game_over = true};
        false ->
            case NewHead =:= Food of
                true ->
                    % Ate food, grow snake
                    NewSnake = [NewHead | Snake],
                    NewFood = spawn_food(NewSnake),
                    State#state{snake = NewSnake, food = NewFood};
                false ->
                    % Normal move
                    NewSnake = [NewHead | lists:droplast(Snake)],
                    State#state{snake = NewSnake}
            end
    end.

-spec move({0..7, 0..7}, up | down | left | right) -> {0..7, 0..7}.
move({X, Y}, up) -> {X, wrap(Y - 1)};
move({X, Y}, down) -> {X, wrap(Y + 1)};
move({X, Y}, left) -> {wrap(X - 1), Y};
move({X, Y}, right) -> {wrap(X + 1), Y}.

-spec wrap(integer()) -> 0..7.
-dialyzer({no_match, wrap/1}).
wrap(N) when N < 0 -> 7;
wrap(N) when N > 7 -> 0;
wrap(N) when N >= 0, N =< 7 -> N.

-spec check_collision({0..7, 0..7}, [{0..7, 0..7}]) -> boolean().
check_collision(Head, Snake) ->
    lists:member(Head, Snake).

-spec spawn_food([{0..7, 0..7}]) -> {0..7, 0..7}.
spawn_food(Snake) ->
    {X, Y} = {rand:uniform(8) - 1, rand:uniform(8) - 1},
    case lists:member({X, Y}, Snake) of
        true -> spawn_food(Snake);
        false -> {X, Y}
    end.

-spec draw(#state{}) -> ok.
draw(#state{snake = Snake, food = {FX, FY}}) ->
    % Clear display
    _ = sensehat:clear(?BLACK),
    
    % Draw snake
    lists:foreach(
        fun({X, Y}) ->
            _ = sensehat:set_pixel(X, Y, ?GREEN),
            ok
        end,
        Snake
    ),
    
    % Draw food
    _ = sensehat:set_pixel(FX, FY, ?RED),
    ok.

-spec flash_game_over() -> ok.
flash_game_over() ->
    _ = sensehat:clear(?RED),
    timer:sleep(200),
    _ = sensehat:clear(?BLACK),
    timer:sleep(200),
    ok.
