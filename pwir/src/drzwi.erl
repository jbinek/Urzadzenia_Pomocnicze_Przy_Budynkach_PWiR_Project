-module(drzwi).
-export([start/0, stop/0]).

% air_conditioning symuluje zachowanie klimatyzacji

port() -> 8088.
id() -> drzwi.


% START
% Rejestruje id drzwi w centrum kontroli na serwerze
% Zaczyna działanie klimatyzacji na podanym porcie

start() ->
  try
    io:format("Drzwi uruchamiaja sie, ID = ~p...~n", [id()]),
    czujniki_UDP:register(centrum_kontroli:address(), centrum_kontroli:port(), id(), port()),
    kontroler_pid:register(id(), self()),
    nasluchuj(),
    start
  catch
    _:_ -> io:format("Za duzo procesow przypisanych do jednego czujnika!~n", []),
      error
  end.

% STOP
% Zatrzymuje drzwi

stop() ->
  try
    czujniki_UDP:unregister(centrum_kontroli:address(), centrum_kontroli:port(), id()),
    io:format("Drzwi o ID = ~p koncza prace ~n", [id()]),
    kontroler_pid:kill(id())
  catch
    _:_ -> io:format("Drzwi nie sa uruchomione!~n"),
      error
  end.

% NASLUCHUJ
% Czeka na informację o otworzeniu lub zamknieciu drzwi
nasluchuj() ->
  case klient_UDP:nasluchuj(port()) of
    {_, _, on} ->
      io:format("Drzwi otwieraja sie ~n");
    {_, _, off} ->
      io:format("Drzwi zamykaja sie ~n");
    _ ->
      nil
  end,
  nasluchuj().
