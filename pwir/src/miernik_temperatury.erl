-module(miernik_temperatury).
-export([start/0, stop/0]).


% symuluje działanie miernika temperatury


id() -> miernik_temp.



% rejestruje miernik na serwerze, przypisuje ID, uruchamia miernik na porcie
start() ->
    try
        io:format("Miernik temperatury uruchamia sie, ID = ~p...~n", [id()]),
        czujniki_UDP:register(centrum_kontroli:address(), centrum_kontroli:port(), id(), 0),
        kontroler_pid:register(id(), self()),
        emit()
    catch
        _:_ -> io:format("Za duzo procesow przypisanych do jednego miernika!~n", []),
            error
    end.

% kończy pracę miernika
stop() ->
    try
        czujniki_UDP:unregister(centrum_kontroli:address(), centrum_kontroli:port(), id()),
        io:format("Miernik temperatury o ID = ~p konczy pracę! ~n", [id()]),
        kontroler_pid:kill(id())
    catch
        _:_ -> io:format("zaden miernik nie jest uruchomiony!~n"),
            error
    end.

% losuje i przekazuje wylosowana wartość temperatury do centrum_kontroli
emit() ->
    czujniki_UDP:sendData(centrum_kontroli:address(), centrum_kontroli:port(), id(), rand:uniform(40)),
    timer:sleep(timer:seconds(5)),
    emit().
