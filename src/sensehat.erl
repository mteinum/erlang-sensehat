%%% @doc Raspberry PI Sense Hat Port integration
%%% This module provides an interface to control the Sense Hat LED display
%%% via a C port driver.
%%% @end

-module(sensehat).
-author("Morten Teinum <morten.teinum@gmail.com>").

-export([start/0, stop/0, init/0]).
-export([set_pixel/3,
         set_pixels/1,
         clear/0,
         clear/1,
         get_gamma/0,
         set_gamma/1,
         set_gamma_low_light/0,
         reset_gamma/1,
         set_rotation/1]).

-type rgb() :: 0..16777215.
-type gamma_type() :: gamma_default | gamma_low.
-type rotation() :: 0 | 90 | 180 | 270.

-spec start() -> pid().
start() ->
    DriverPath = case code:priv_dir(?MODULE) of
        {error, bad_name} -> ".";
        Dir -> Dir
    end,
    case erl_ddll:load_driver(DriverPath, sensehat_drv) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Message} -> exit(erl_ddll:format_error(Message))
    end,
    spawn(?MODULE, init, []).

-spec stop() -> ok.
stop() ->
    case whereis(sensehat) of
        undefined -> ok;
        _Pid -> sensehat ! stop, ok
    end.

-spec init() -> no_return().
init() ->
    % Register this process with the atom sensehat
    % self() will be the port owner
    register(sensehat, self()),

    % Open and start the driver
    Port = open_port({spawn_driver, sensehat_drv}, [binary]),

    % Start waiting for messages from the port
    loop(Port, shfb:create(0)).

loop(Port, FB) ->
    Port ! {self(), {command, [1, shfb:to_binary(FB)]}},

    % Wait for work to do
    receive
        {set_pixel, X, Y, RGB} ->
            loop(Port, shfb:set_pixel(X, Y, RGB, FB));

        {set_pixels, Pixels} ->
            loop(Port, shfb:set_pixels(Pixels, FB));
        
        {clear, RGB} ->
            loop(Port, shfb:create(RGB));

        {set_rotation, N} ->
            loop(Port, shfb:set_rotation(N, FB));

        {cast, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            loop(Port, FB);

        {call, Caller, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {sensehat, Data}
            end,
            loop(Port, FB);

        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            after 5000 ->
                    exit(timeout)
            end;

        {'EXIT', Port, Reason} ->
            error_logger:error_msg("Port terminated: ~p~n", [Reason]),
            exit(port_terminated);

        Other ->
            error_logger:warning_msg("Unknown message: ~p~n", [Other]),
            loop(Port, FB)
    end.

encode({get_gamma})                  -> [2];
encode({set_gamma, Value})           -> [3, Value];
encode({reset_gamma, gamma_default}) -> [4, 0];
encode({reset_gamma, gamma_low})     -> [4, 1].

cast(Msg) ->
	sensehat ! {cast, Msg},
	ok.

call(Msg) ->
  	sensehat ! {call, self(), Msg},
 	receive
 		{sensehat, Result} ->
 			Result
 	end.	

%%%
%%% API
%%%

-spec reset_gamma(gamma_type()) -> ok | {error, not_running}.
reset_gamma(Type) ->
    cast({reset_gamma, Type}).

-spec get_gamma() -> binary() | {error, not_running}.
get_gamma() ->
    call({get_gamma}).

-spec set_gamma(binary()) -> ok | {error, not_running}.
set_gamma(Value) ->
    cast({set_gamma, Value}).

-spec set_gamma_low_light() -> ok | {error, not_running}.
set_gamma_low_light() ->
    set_gamma(<<0,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,3,3,3,4,4,5,5,6,6,7,7,8,8,9,10,10>>).

-spec set_pixel(0..7, 0..7, rgb()) -> ok | {error, not_running}.
set_pixel(X, Y, RGB) when X >= 0, X =< 7, Y >= 0, Y =< 7 ->
    case whereis(sensehat) of
        undefined -> {error, not_running};
        _Pid -> sensehat ! {set_pixel, X, Y, RGB}, ok
    end.

-spec set_pixels([[rgb()]]) -> ok | {error, not_running}.
set_pixels(Pixels) when length(Pixels) =:= 8 ->
    case whereis(sensehat) of
        undefined -> {error, not_running};
        _Pid -> sensehat ! {set_pixels, Pixels}, ok
    end.

-spec set_rotation(rotation()) -> ok | {error, not_running}.
set_rotation(N) when N =:= 0; N =:= 90; N =:= 180; N =:= 270 ->
    case whereis(sensehat) of
        undefined -> {error, not_running};
        _Pid -> sensehat ! {set_rotation, N}, ok
    end.

-spec clear(rgb()) -> ok | {error, not_running}.
clear(RGB) ->
    case whereis(sensehat) of
        undefined -> {error, not_running};
        _Pid -> sensehat ! {clear, RGB}, ok
    end.

-spec clear() -> ok | {error, not_running}.
clear() ->
    clear(16#000000).
