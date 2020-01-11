-module(air_conditioning).
-export([start/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%
%% air_conditioning simulates behavior of air conditioner controller.
%% Functions: start, stop, listen
%%%%%%%%%%%%%%%%%%%%%%

port() -> 8081.
id() -> ac.


%%%%%%%%%%%%%%%%%%%%%%
%% Function start
%% Registers air conditioner controller on the server,
%% Starts the air conditioner controller on the given port.
%%%%%%%%%%%%%%%%%%%%%%
start() ->
    try
        io:format("Starting air conditioner controller with Id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), port()),
        process_manager:register(id(), self()),
        listen(),
        start
    catch
        _:_ -> io:format("Single process may handle only one air conditioner controller!~n", []),
        error
    end.

  %%%%%%%%%%%%%%%%%%%%%%
  %% Function stop
  %% Stops the air conditioner controller.
  %%%%%%%%%%%%%%%%%%%%%%
stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Air conditioner controller which Id is ~p is being stopped ~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("There are no air conditioner controllers working on this process!~n"),
        error
    end.

  %%%%%%%%%%%%%%%%%%%%%%
  %% Function listen
  %% Waits for information whether air conditioner controller should be switched on or off.
  %%%%%%%%%%%%%%%%%%%%%%

listen() ->
    case consumer_utils:listen(port()) of
        {_, _, on} ->
            io:format("Turning air conditioning on ~n");
        {_, _, off} ->
            io:format("Turning air conditioning off ~n");
        _ ->
            nil
    end,
    listen().
