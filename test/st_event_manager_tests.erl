%%% @doc Unit tests for st_event_manager module
%%% @end

-module(st_event_manager_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Setup and Teardown
%%%===================================================================

event_manager_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          {"subscribe returns handler id", fun() -> test_subscribe(Pid) end},
          {"unsubscribe removes handler", fun() -> test_unsubscribe(Pid) end},
          {"notify sends events", fun() -> test_notify(Pid) end}
         ]
     end}.

setup() ->
    % Start the event manager for testing
    {ok, Pid} = gen_event:start_link(),
    Pid.

cleanup(Pid) ->
    gen_event:stop(Pid).

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_subscribe(Pid) ->
    HandlerId = gen_event:add_handler(Pid, st_event, [self()]),
    ?assertEqual(ok, HandlerId),
    % Clean up after ourselves
    gen_event:delete_handler(Pid, st_event, cleanup).

test_unsubscribe(Pid) ->
    % Ensure no handler is present first
    lists:foreach(fun(_) -> 
        gen_event:delete_handler(Pid, st_event, cleanup)
    end, gen_event:which_handlers(Pid)),
    
    ok = gen_event:add_handler(Pid, st_event, [self()]),
    Handlers = gen_event:which_handlers(Pid),
    ?assert(lists:member(st_event, Handlers)),
    
    % Delete handler and verify removal
    ok = gen_event:delete_handler(Pid, st_event, shutdown),
    NewHandlers = gen_event:which_handlers(Pid),
    ?assertNot(lists:member(st_event, NewHandlers)).

test_notify(Pid) ->
    ok = gen_event:add_handler(Pid, st_event, [self()]),
    
    % Test notification
    ok = gen_event:notify(Pid, up),
    
    % Should receive the event
    receive
        {st_event, up} -> ok
    after 100 ->
        ?assert(false) % Should have received event
    end.
