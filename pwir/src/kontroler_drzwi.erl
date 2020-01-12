-module(kontroler_drzwi).
-export([start/0, stop/0]).

% symuluje zachowanie kontrolera drzwi

id() -> kontroler_drzwi.

% START
% Rejestruje id kontrolera_drzwi w centrum kontroli na serwerze
% Zaczyna działanie kontrolera na podanym porcie

start() ->
  try
      io:format("Kontroler drzwi uruchamia sie, ID = ~p...~n", [id()]),
      czujniki_UDP:register(centrum_kontroli:address(), centrum_kontroli:port(), id(), 0),
      kontroler_pid:register(id(), self()),
      emit()
  catch
      _:_ -> io:format("Za duzo procesow przypisanych do jednego kolntrolera!~n", []),
          error
  end.

% STOP
% Zatrzymuje kontroler drzwi

stop() ->
  try
      czujniki_UDP:unregister(centrum_kontroli:address(), centrum_kontroli:port(), id()),
      io:format("Kontroler drzwi o ID = ~p kończy pracę ~n", [id()]),
      kontroler_pid:kill(id())
  catch
      _:_ -> io:format("Kontroler drzwi nie jest uruchomiony!~n"),
          error
  end.

% EMIT
% Kontroler co 5 sekund losuje godiznę i wysyla do centrum kontroli informacje o zamknięciu drzwi

emit() ->
    czujniki_UDP:sendData(centrum_kontroli:address(), centrum_kontroli:port(), id(), rand:uniform(24)),
    timer:sleep(timer:seconds(5)),
    emit().
