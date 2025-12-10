%%% @doc Sense Hat Event Handler
%%% Generic event handler that forwards joystick events to a subscriber process.
%%% @end

-module(st_event).
-behaviour(gen_event).

-export([code_change/3, handle_call/2, handle_event/2, handle_info/2, init/1, terminate/2]).

-spec init([pid()]) -> {ok, pid()}.
init([Pid]) when is_pid(Pid) ->
    {ok, Pid}.

-spec handle_event(term(), pid()) -> {ok, pid()}.
handle_event(Event, Pid) ->
    Pid ! {st_event, Event},
    {ok, Pid}.

-spec handle_call(term(), pid()) -> {ok, ok, pid()}.
handle_call(_Request, State) ->
    {ok, ok, State}.

-spec handle_info(term(), pid()) -> {ok, pid()}.
handle_info(_Info, State) ->
    {ok, State}.

-spec terminate(term(), pid()) -> ok.
terminate(_Arg, _State) ->
    ok.

-spec code_change(term(), pid(), term()) -> {ok, pid()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.