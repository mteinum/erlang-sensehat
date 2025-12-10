%%% @doc Sense Hat Joystick Supervisor
%%% Supervises the event manager and port driver for the joystick.
%%% Uses one_for_one strategy: if one child crashes, only that child is restarted.
%%% @end

-module(st_supervisor).
-behaviour(supervisor).
-export([init/1, start_link/0]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_Args) ->
    %% Restart strategy: one_for_one
    %% Max 5 restarts in 60 seconds
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    %% Child specifications
    Children = [
        #{
            id => st_event_manager,
            start => {st_event_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [st_event_manager]
        },
        #{
            id => st_port,
            start => {st_port, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [st_port]
        }
    ],

    {ok, {SupFlags, Children}}.