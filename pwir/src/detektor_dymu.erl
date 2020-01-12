-module(detektor_dymu).
-export([start/0, stop/0]).

% symuluje działanie detektora dymu

id() -> detektor.

% rejestruje detektor na serwerze, przypisuje ID, uruchamia detektor na porcie
start() ->
    try
        io:format("Detektor dymu uruchamia sie, ID = ~p...~n", [id()]),
        czujniki_UDP:register(centrum_kontroli:address(), centrum_kontroli:port(), id(), 0),
        kontroler_pid:register(id(), self()),
        emit()
    catch
        _:_ -> io:format("Za duzo procesow przypisanych do jednego detektora!~n", []),
            error
    end.

% kończy pracę detektora
stop() ->
    try
        czujniki_UDP:unregister(centrum_kontroli:address(), centrum_kontroli:port(), id()),
        io:format("Detektor dymu o ID = ~p konczy prace! ~n", [id()]),
        kontroler_pid:kill(id())
    catch
        _:_ -> io:format("Zaden detektor nie jest uruchomiony!~n"),
            error
    end.

% co 10 sekund losuje czy wyslac powiadomienie do centrum kontroli o wykryciu dymu czy nie
emit() ->
    case rand:uniform(2) of
        1 ->
            czujniki_UDP:sendData(centrum_kontroli:address(), centrum_kontroli:port(), id(), tak);
        _ ->
            czujniki_UDP:sendData(centrum_kontroli:address(), centrum_kontroli:port(), id(), no)
    end,
    timer:sleep(timer:seconds(10)),
    emit().
