%%% @doc Erlang Raspberry Pi Sense Hat Examples
%%% Collection of example patterns and animations for the LED display.
%%% @end

-module(shexample).
-author("Morten Teinum <morten.teinum@gmail.com>").
-export([
    % Basic patterns
    logo/0, rainbow/0, checkerboard/0, red_cross/0,
    all_off/0, all_on/0, border/0, corner_pixels/0,
    % Faces and symbols
    smiley/0, sad_face/0, heart/0,
    % Arrows
    arrow_up/0, arrow_down/0, arrow_left/0, arrow_right/0,
    % Interactive
    traffic_light/1, letter_e/0,
    % Animations
    blink/1, blink/2, demo_sequence/0
]).

%% Color constants
-define(BLACK,   16#000000).
-define(WHITE,   16#ffffff).
-define(RED,     16#ff0000).
-define(GREEN,   16#00ff00).
-define(BLUE,    16#0000ff).
-define(YELLOW,  16#ffff00).
-define(CYAN,    16#00ffff).
-define(MAGENTA, 16#ff00ff).

-spec logo() -> ok | {error, not_running}.
logo() ->
    sensehat:set_pixels(
        [[16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff],
         [16#fcf2f5, 16#ffffff, 16#fdf9fa, 16#eabec4, 16#f9e9ee, 16#ffffff, 16#fbeff2, 16#e9b1c1],
         [16#fdf7f9, 16#ffffff, 16#e192a7, 16#a70712, 16#da7a94, 16#ffffff, 16#fdf7f8, 16#cc4b6e],
         [16#ffffff, 16#ffffff, 16#fef9fa, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#e39eb1],
         [16#ffffff, 16#ffffff, 16#da7d97, 16#ca5b69, 16#d1577d, 16#c94268, 16#bc2342, 16#c93f65],
         [16#ffffff, 16#ffffff, 16#cc4a6e, 16#930000, 16#9e0000, 16#a60005, 16#bc2444, 16#c73b61],
         [16#f8e6eb, 16#ffffff, 16#fbeff2, 16#c43658, 16#b31d2a, 16#e6a6b8, 16#ffffff, 16#d56886],
         [16#f8e6ea, 16#ffffff, 16#ffffff, 16#ffffff, 16#fffdff, 16#ffffff, 16#fefbfc, 16#f0c9d4]]).

-spec rainbow() -> ok | {error, not_running}.
rainbow() ->
    sensehat:set_pixels(
        [[?RED,     ?RED,     ?YELLOW,  ?YELLOW,  ?GREEN,   ?GREEN,   ?CYAN,    ?CYAN],
         [?RED,     ?RED,     ?YELLOW,  ?YELLOW,  ?GREEN,   ?GREEN,   ?CYAN,    ?CYAN],
         [?YELLOW,  ?YELLOW,  ?GREEN,   ?GREEN,   ?CYAN,    ?CYAN,    ?BLUE,    ?BLUE],
         [?YELLOW,  ?YELLOW,  ?GREEN,   ?GREEN,   ?CYAN,    ?CYAN,    ?BLUE,    ?BLUE],
         [?GREEN,   ?GREEN,   ?CYAN,    ?CYAN,    ?BLUE,    ?BLUE,    ?MAGENTA, ?MAGENTA],
         [?GREEN,   ?GREEN,   ?CYAN,    ?CYAN,    ?BLUE,    ?BLUE,    ?MAGENTA, ?MAGENTA],
         [?CYAN,    ?CYAN,    ?BLUE,    ?BLUE,    ?MAGENTA, ?MAGENTA, ?RED,     ?RED],
         [?CYAN,    ?CYAN,    ?BLUE,    ?BLUE,    ?MAGENTA, ?MAGENTA, ?RED,     ?RED]]).

-spec checkerboard() -> ok | {error, not_running}.
checkerboard() ->
    sensehat:set_pixels(
        [[?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK],
         [?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE],
         [?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK],
         [?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE],
         [?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK],
         [?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE],
         [?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK],
         [?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE, ?BLACK, ?WHITE]]).

-spec red_cross() -> ok | {error, not_running}.
red_cross() ->
    sensehat:set_pixels(
        [[?BLACK, ?BLACK, ?BLACK, ?RED,   ?RED,   ?BLACK, ?BLACK, ?BLACK],
         [?BLACK, ?BLACK, ?BLACK, ?RED,   ?RED,   ?BLACK, ?BLACK, ?BLACK],
         [?BLACK, ?BLACK, ?BLACK, ?RED,   ?RED,   ?BLACK, ?BLACK, ?BLACK],
         [?RED,   ?RED,   ?RED,   ?RED,   ?RED,   ?RED,   ?RED,   ?RED],
         [?RED,   ?RED,   ?RED,   ?RED,   ?RED,   ?RED,   ?RED,   ?RED],
         [?BLACK, ?BLACK, ?BLACK, ?RED,   ?RED,   ?BLACK, ?BLACK, ?BLACK],
         [?BLACK, ?BLACK, ?BLACK, ?RED,   ?RED,   ?BLACK, ?BLACK, ?BLACK],
         [?BLACK, ?BLACK, ?BLACK, ?RED,   ?RED,   ?BLACK, ?BLACK, ?BLACK]]).

-spec all_off() -> ok | {error, not_running}.
all_off() ->
    sensehat:clear(?BLACK).

-spec all_on() -> ok | {error, not_running}.
all_on() ->
    sensehat:clear(?WHITE).

-spec blink(pos_integer()) -> ok.
blink(Times) ->
    blink(Times, 500).

-spec blink(pos_integer(), pos_integer()) -> ok.
blink(0, _Delay) ->
    _ = all_off(),
    ok;
blink(Times, Delay) ->
    _ = all_on(),
    timer:sleep(Delay),
    _ = all_off(),
    timer:sleep(Delay),
    blink(Times - 1, Delay).

-spec corner_pixels() -> ok | {error, not_running}.
corner_pixels() ->
    sensehat:set_pixels(
        [[?RED,    ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?GREEN],
         [?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK],
         [?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK],
         [?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK],
         [?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK],
         [?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK],
         [?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK],
         [?BLUE,  ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?YELLOW]]).

-spec border() -> ok | {error, not_running}.
border() ->
    sensehat:set_pixels(
        [[?WHITE, ?WHITE, ?WHITE, ?WHITE, ?WHITE, ?WHITE, ?WHITE, ?WHITE],
         [?WHITE, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?WHITE],
         [?WHITE, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?WHITE],
         [?WHITE, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?WHITE],
         [?WHITE, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?WHITE],
         [?WHITE, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?WHITE],
         [?WHITE, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?BLACK, ?WHITE],
         [?WHITE, ?WHITE, ?WHITE, ?WHITE, ?WHITE, ?WHITE, ?WHITE, ?WHITE]]).

-spec smiley() -> ok | {error, not_running}.
smiley() ->
    Y = ?YELLOW,
    B = ?BLACK,
    sensehat:set_pixels(
        [[B, B, Y, Y, Y, Y, B, B],
         [B, Y, B, B, B, B, Y, B],
         [Y, B, Y, B, B, Y, B, Y],
         [Y, B, B, B, B, B, B, Y],
         [Y, B, Y, B, B, Y, B, Y],
         [Y, B, B, Y, Y, B, B, Y],
         [B, Y, B, B, B, B, Y, B],
         [B, B, Y, Y, Y, Y, B, B]]).

-spec sad_face() -> ok | {error, not_running}.
sad_face() ->
    Y = ?YELLOW,
    B = ?BLACK,
    sensehat:set_pixels(
        [[B, B, Y, Y, Y, Y, B, B],
         [B, Y, B, B, B, B, Y, B],
         [Y, B, Y, B, B, Y, B, Y],
         [Y, B, B, B, B, B, B, Y],
         [Y, B, B, Y, Y, B, B, Y],
         [Y, B, Y, B, B, Y, B, Y],
         [B, Y, B, B, B, B, Y, B],
         [B, B, Y, Y, Y, Y, B, B]]).

-spec heart() -> ok | {error, not_running}.
heart() ->
    R = ?RED,
    B = ?BLACK,
    sensehat:set_pixels(
        [[B, R, R, B, B, R, R, B],
         [R, R, R, R, R, R, R, R],
         [R, R, R, R, R, R, R, R],
         [R, R, R, R, R, R, R, R],
         [B, R, R, R, R, R, R, B],
         [B, B, R, R, R, R, B, B],
         [B, B, B, R, R, B, B, B],
         [B, B, B, B, B, B, B, B]]).

-spec arrow_up() -> ok | {error, not_running}.
arrow_up() ->
    W = ?WHITE,
    B = ?BLACK,
    sensehat:set_pixels(
        [[B, B, B, W, W, B, B, B],
         [B, B, W, W, W, W, B, B],
         [B, W, B, W, W, B, W, B],
         [B, B, B, W, W, B, B, B],
         [B, B, B, W, W, B, B, B],
         [B, B, B, W, W, B, B, B],
         [B, B, B, W, W, B, B, B],
         [B, B, B, W, W, B, B, B]]).

-spec arrow_down() -> ok | {error, not_running}.
arrow_down() ->
    W = ?WHITE,
    B = ?BLACK,
    sensehat:set_pixels(
        [[B, B, B, W, W, B, B, B],
         [B, B, B, W, W, B, B, B],
         [B, B, B, W, W, B, B, B],
         [B, B, B, W, W, B, B, B],
         [B, B, B, W, W, B, B, B],
         [B, W, B, W, W, B, W, B],
         [B, B, W, W, W, W, B, B],
         [B, B, B, W, W, B, B, B]]).

-spec arrow_left() -> ok | {error, not_running}.
arrow_left() ->
    W = ?WHITE,
    B = ?BLACK,
    sensehat:set_pixels(
        [[B, B, B, W, B, B, B, B],
         [B, B, W, W, B, B, B, B],
         [B, W, B, W, B, B, B, B],
         [W, W, W, W, W, W, W, W],
         [W, W, W, W, W, W, W, W],
         [B, W, B, W, B, B, B, B],
         [B, B, W, W, B, B, B, B],
         [B, B, B, W, B, B, B, B]]).

-spec arrow_right() -> ok | {error, not_running}.
arrow_right() ->
    W = ?WHITE,
    B = ?BLACK,
    sensehat:set_pixels(
        [[B, B, B, B, W, B, B, B],
         [B, B, B, B, W, W, B, B],
         [B, B, B, B, W, B, W, B],
         [W, W, W, W, W, W, W, W],
         [W, W, W, W, W, W, W, W],
         [B, B, B, B, W, B, W, B],
         [B, B, B, B, W, W, B, B],
         [B, B, B, B, W, B, B, B]]).

-spec traffic_light(green | yellow | red) -> ok | {error, not_running}.
traffic_light(green) ->
    G = ?GREEN,
    B = ?BLACK,
    sensehat:set_pixels(
        [[B, B, B, B, B, B, B, B],
         [B, B, B, B, B, B, B, B],
         [B, B, B, B, B, B, B, B],
         [B, B, B, B, B, B, B, B],
         [B, B, B, B, B, B, B, B],
         [B, B, B, G, G, B, B, B],
         [B, B, G, G, G, G, B, B],
         [B, B, B, G, G, B, B, B]]);
traffic_light(yellow) ->
    Y = ?YELLOW,
    B = ?BLACK,
    sensehat:set_pixels(
        [[B, B, B, B, B, B, B, B],
         [B, B, B, B, B, B, B, B],
         [B, B, B, B, B, B, B, B],
         [B, B, B, Y, Y, B, B, B],
         [B, B, Y, Y, Y, Y, B, B],
         [B, B, B, Y, Y, B, B, B],
         [B, B, B, B, B, B, B, B],
         [B, B, B, B, B, B, B, B]]);
traffic_light(red) ->
    R = ?RED,
    B = ?BLACK,
    sensehat:set_pixels(
        [[B, B, B, R, R, B, B, B],
         [B, B, R, R, R, R, B, B],
         [B, B, B, R, R, B, B, B],
         [B, B, B, B, B, B, B, B],
         [B, B, B, B, B, B, B, B],
         [B, B, B, B, B, B, B, B],
         [B, B, B, B, B, B, B, B],
         [B, B, B, B, B, B, B, B]]).

-spec letter_e() -> ok | {error, not_running}.
letter_e() ->
    W = ?WHITE,
    B = ?BLACK,
    sensehat:set_pixels(
        [[B, B, B, B, B, B, B, B],
         [B, W, W, W, W, W, W, B],
         [B, W, W, B, B, B, B, B],
         [B, W, W, W, W, W, B, B],
         [B, W, W, B, B, B, B, B],
         [B, W, W, B, B, B, B, B],
         [B, W, W, W, W, W, W, B],
         [B, B, B, B, B, B, B, B]]).

-spec demo_sequence() -> ok.
demo_sequence() ->
    Demos = [
        fun logo/0,
        fun smiley/0,
        fun heart/0,
        fun rainbow/0,
        fun checkerboard/0,
        fun red_cross/0,
        fun border/0,
        fun arrow_up/0,
        fun arrow_right/0,
        fun arrow_down/0,
        fun arrow_left/0,
        fun traffic_light/1
    ],
    lists:foreach(
        fun(Fun) ->
            case Fun of
                TrafficFn when is_function(TrafficFn, 1) ->
                    TrafficFn(red),
                    timer:sleep(800),
                    TrafficFn(yellow),
                    timer:sleep(800),
                    TrafficFn(green),
                    timer:sleep(800);
                _ ->
                    Fun(),
                    timer:sleep(1500)
            end
        end,
        Demos
    ),
    all_off().