-module(anti_intrusion_sensor).
-export([start/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%
%% anti_intrusion_sensor simulates behavior of anti intrusion sensor.
%% Functions: start, stop, emit
%%%%%%%%%%%%%%%%%%%%%%

id() -> intrusion.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: start
%% Registers anti intrusion sensor on the server,
%% Starts the anti intrusion sensor on the given port.
%%%%%%%%%%%%%%%%%%%%%%

start() ->
    try
        io:format("Starting anti intrusion sensor with Id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), 0),
        process_manager:register(id(), self()),
        emit()
    catch
        _:_ -> io:format("Single process may handle only one anti intrusion sensor!~n", []),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: stop
%% Stops the anti intrusion sensor sensor.
%%%%%%%%%%%%%%%%%%%%%%

stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Anti intrusion sensor which Id is ~p is being stopped~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("There are no anti intrusion sensors working on this process!~n"),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: emit
%% Every 10 seconds sends to controller information whether intrusion has occurred.
%%%%%%%%%%%%%%%%%%%%%%

emit() ->
    case rand:uniform(1) of
        1 ->
            emitter_utils:sendData(controller:address(), controller:port(), id(), yes);
        _ ->
        emitter_utils:sendData(controller:address(), controller:port(), id(), no)
    end,
    timer:sleep(timer:seconds(10)),
    emit().
