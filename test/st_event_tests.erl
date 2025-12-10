%%% @doc Unit tests for st_event module
%%% @end

-module(st_event_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Test event handler initialization
init_test() ->
    Pid = self(),
    {ok, State} = st_event:init([Pid]),
    ?assertEqual(Pid, State).

%% Test event handling
handle_event_test() ->
    State = self(),
    
    % Test joystick events
    Events = [up, down, left, right, enter],
    lists:foreach(fun(Event) ->
        {ok, NewState} = st_event:handle_event(Event, State),
        ?assertEqual(State, NewState),
        % Verify message was sent
        receive
            {st_event, ReceivedEvent} ->
                ?assertEqual(Event, ReceivedEvent)
        after 100 ->
            ?assert(false) % Should have received message
        end
    end, Events).

handle_event_unknown_test() ->
    State = self(),
    {ok, NewState} = st_event:handle_event(unknown_event, State),
    ?assertEqual(State, NewState),
    % Verify message was sent
    receive
        {st_event, unknown_event} ->
            ok
    after 100 ->
        ?assert(false)
    end.

%% Test handler termination
terminate_test() ->
    State = self(),
    ?assertEqual(ok, st_event:terminate(normal, State)),
    ?assertEqual(ok, st_event:terminate({error, reason}, State)).

%% Test code change
code_change_test() ->
    State = self(),
    {ok, NewState} = st_event:code_change(old_vsn, State, extra),
    ?assertEqual(State, NewState).
