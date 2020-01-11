-module(smoke_sensor).
-export([start/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%
%% smoke_sensor simulates behavior of smoke sensor.
%% Functions: start, stop, emit
%%%%%%%%%%%%%%%%%%%%%%

id() -> smoke.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: start
%% Registers smoke sensor on the server,
%% Starts the smoke sensor on the given port.
%%%%%%%%%%%%%%%%%%%%%%
start() ->
    try
        io:format("Starting smoke senors with Id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), 0),
        process_manager:register(id(), self()),
        emit()
    catch
        _:_ -> io:format("Single process may handle only one smoke sensor!~n", []),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: stop
%% Stops the smoke sensor.
%%%%%%%%%%%%%%%%%%%%%%

stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("smoke sensor which Id is ~p is being stopped ~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("There are no smoke sensors working on this process!~n"),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: emit
%% Every 10 seconds sends to controller information whether smoke has been detected.
%%%%%%%%%%%%%%%%%%%%%%

emit() ->
    case rand:uniform(2) of
        1 ->
        emitter_utils:sendData(controller:address(), controller:port(), id(), yes);
        _ ->
        emitter_utils:sendData(controller:address(), controller:port(), id(), no)
    end,
    timer:sleep(timer:seconds(10)),
    emit().
