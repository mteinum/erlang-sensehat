%%% @doc Sense Hat Event Manager
%%% Provides event subscription management for joystick events.
%%% @end

-module(st_event_manager).
-export([start_link/0, subscribe/0, unsubscribe/1]).

-type handler_id() :: {st_event, reference()}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_event:start_link({local, ?MODULE}).

-spec subscribe() -> handler_id().
subscribe() ->
    HandlerId = {st_event, make_ref()},
    ok = gen_event:add_handler(st_event_manager, HandlerId, [self()]),
    HandlerId.

-spec unsubscribe(handler_id()) -> ok | {error, term()}.
unsubscribe(HandlerId) ->
    gen_event:delete_handler(st_event_manager, HandlerId, []).