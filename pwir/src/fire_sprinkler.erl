-module(fire_sprinkler).
-export([start/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%
%% fire_sprinkler simulates behavior of fire sprinkler controller.
%% Functions: start, stop, listen
%%%%%%%%%%%%%%%%%%%%%%

port() -> 8089.
id() -> sprinkler.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: start
%% Registers fire sprinkler controller on the server,
%% Starts the alarm sensor on the given port.
%%%%%%%%%%%%%%%%%%%%%%


start() ->
    try
        io:format("Starting fire sprinkler controller with Id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), port()),
        process_manager:register(id(), self()),
        listen(),
        start
    catch
        _:_ -> io:format("Single process may handle only one fire sprinkler controller!~n", []),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: stop
%% Stops the fire sprinkler controller.
%%%%%%%%%%%%%%%%%%%%%%

stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Fire sprinkler controller which Id is ~p is being stopped~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("There are no fire sprinkler controllers working on this process!~n"),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: listen
%% Waits for information whether fire sprinkler should be turned on or off.
%%%%%%%%%%%%%%%%%%%%%%

listen() ->
    case consumer_utils:listen(port()) of
        {_, _, on} ->
            io:format("Fire sprinkler is being turned on ~n");
        {_, _, off} ->
            io:format("Fire sprinkler is being turned off ~n");
        _ ->
            nil
    end,
    listen().
