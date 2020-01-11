-module(temperature_sensor).
-export([start/0, stop/0]).


% symuluje działanie miernika temperatury


id() -> temp.



% rejestruje miernik na serwerze, przypisuje ID, uruchamia miernik na porcie
start() ->
    try
        io:format("Miernik temperatury uruchamia się, ID = ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), 0),
        process_manager:register(id(), self()),
        emit()
    catch
        _:_ -> io:format("Za duzo procesow przypisanych do jednego miernika!~n", []),
            error
    end.

% kończy pracę miernika
stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Miernik temperatury o ID = ~p kończy pracę! ~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("Żaden miernik nie jest uruchomiony!~n"),
            error
    end.

% losuje i przekazuje wylosowana wartość temperatury do centrum_kontroli
emit() ->
    emitter_utils:sendData(controller:address(), controller:port(), id(), rand:uniform(40)),
    timer:sleep(timer:seconds(5)),
    emit().
