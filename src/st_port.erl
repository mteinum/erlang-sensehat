%%% @doc Sense Hat Joystick Port Driver
%%% Manages the connection to the joystick hardware via port driver
%%% and publishes events to the event manager.
%%% @end

-module(st_port).
-author("Morten Teinum <morten.teinum@gmail.com>").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-type event() :: up | down | left | right | enter.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    DriverPath = case code:priv_dir(?MODULE) of
        {error, bad_name} -> ".";
        Dir -> Dir
    end,
    case erl_ddll:load_driver(DriverPath, sensestick_drv) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Message} -> exit(erl_ddll:format_error(Message))
    end,
    gen_server:start_link(?MODULE, [], []).

-spec init(list()) -> {ok, port()}.
init(_Args) ->
    %% Connect to the port driver
    Port = open_port({spawn_driver, sensestick_drv}, []),
    %% Send control message to connect to the joystick
    port_control(Port, 1, []),
    {ok, Port}.

-spec handle_call(term(), {pid(), term()}, port()) -> {reply, {error, unknown_call}, port()}.
handle_call(_Request, _From, Port) ->
    {reply, {error, unknown_call}, Port}.

-spec handle_cast(term(), port()) -> {noreply, port()} | {stop, normal, port()}.
handle_cast(shutdown, Port) ->
    {stop, normal, Port};
handle_cast(_Msg, Port) ->
    {noreply, Port}.

-spec handle_info(term(), port()) -> {noreply, port()}.
handle_info({_Port, {data, [Code]}}, Port) ->
    case code_to_event(Code) of
        {ok, Event} -> gen_event:notify(st_event_manager, Event);
        {error, _} -> ok
    end,
    {noreply, Port};
handle_info(_Info, Port) ->
    {noreply, Port}.

-spec terminate(term(), port()) -> ok.
terminate(Reason, Port) ->
    error_logger:info_msg("st_port terminating: ~p~n", [Reason]),
    catch port_close(Port),
    ok.

-spec code_change(term(), port(), term()) -> {ok, port()}.
code_change(_OldVsn, Port, _Extra) ->
    {ok, Port}.

-spec code_to_event(pos_integer()) -> {ok, event()} | {error, invalid_code}.
code_to_event(Code) when Code >= 1, Code =< 5 ->
    Events = [up, down, left, right, enter],
    {ok, lists:nth(Code, Events)};
code_to_event(_Code) ->
    {error, invalid_code}.
  	