-module(temperature_sensor).
-export([start/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%
%% temperature_sensor simulates behavior of temperature sensor.
%% Functions: start, stop, emit
%%%%%%%%%%%%%%%%%%%%%%

id() -> temp.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: start
%% Registers temperature sensor on the server,
%% Starts the temperature sensor on the given port.
%%%%%%%%%%%%%%%%%%%%%%
start() ->
    try
        io:format("Starting temperature senor with Id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), 0),
        process_manager:register(id(), self()),
        emit()
    catch
        _:_ -> io:format("Single process may handle only one temperature sensor!~n", []),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: stop
%% Stops the temperature sensor.
%%%%%%%%%%%%%%%%%%%%%%
stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Temperature sensor which Id is ~p is being stopped ~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("There are no temperature sensors working on this process!~n"),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: emit
%% Every 5 seconds sends temperature value to the controller.
%%%%%%%%%%%%%%%%%%%%%%

emit() ->
    emitter_utils:sendData(controller:address(), controller:port(), id(), rand:uniform(40)),
    timer:sleep(timer:seconds(5)),
    emit().
