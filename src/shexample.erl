%%% @doc Erlang Raspberry Pi Sense Hat Examples
%%% Collection of example patterns and animations for the LED display.
%%% @end

-module(shexample).
-author("Morten Teinum <morten.teinum@gmail.com>").
-export([logo/0, rainbow/0, checkerboard/0, red_cross/0, all_off/0, all_on/0]).

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