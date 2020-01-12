-module(rolety).
-export([start/0, stop/0]).

% symuluje zachowanie rolet w oknach

port() -> 8082.
id() -> rolety.


% START
% Rejestruje id rolet w centrum kontroli na serwerze
% Zaczyna działanie rolet na podanym porcie

start() ->
  try
    io:format("Rolety gotowe do uzytku, ID = ~p ~n", [id()]),
    czujniki_UDP:register(centrum_kontroli:address(), centrum_kontroli:port(), id(), port()),
    kontroler_pid:register(id(), self()),
    listen(),
    start
  catch
    _:_ -> io:format("Za duzo procesow przypisanych do jednych rolet!~n", []),
      error
  end.

% STOP
% Zatrzymuje prace rolet

stop() ->
  try
    czujniki_UDP:unregister(centrum_kontroli:address(), centrum_kontroli:port(), id()),
    io:format("Rolety nieaktywne , ID = ~p ~n", [id()]),
    kontroler_pid:kill(id())
  catch
    _:_ -> io:format("Rolety nie sa aktywne! ~n"),
      error
  end.

% LISTEN
% Czeka na informację o włączeniu lub wyłączeniu rolet

listen() ->
  case klient_UDP:listen(port()) of
    {_, _, on} ->
      io:format("Opuszczanie rolet ~n");
    {_, _, off} ->
      io:format("Podnoszenie rolet ~n");
    _ ->
      nil
  end,
  listen().
