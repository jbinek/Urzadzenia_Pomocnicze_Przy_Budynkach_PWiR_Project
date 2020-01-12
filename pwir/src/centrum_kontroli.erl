-module(centrum_kontroli).
-export([start/0, stop/0, port/0, address/0, temperatura/1, dym/1, godzina/1, wlamanie/1]).
% serwer aplikacji, centrum kontroli

port() -> 5000.
address() -> {127,0,0,1}.
id() -> centrum_kontroli.

%uruchamia serwer na porcie 5000
start() ->
    try
        ets:new(clientSet, [set, named_table, public]),
        ets:new(dataSet, [set, named_table, public]),
        ets:new(signalHandlers, [bag, named_table, public]),
        io:format("Uruchomienie serwera na porcie ~p ~n", [port()]),

        ets:insert(signalHandlers, {miernik_temp, fun temperatura/1}),
        ets:insert(signalHandlers, {detektor, fun dym/1}),
        ets:insert(signalHandlers, {kontroler_drzwi, fun godzina/1}),
        ets:insert(signalHandlers, {czujnik_antywlam, fun wlamanie/1}),


        kontroler_pid:register(id(), self()),
        nasluchuj(),
        start
    catch
        A:B -> io:format("Nie mozna uruchomic centrum kontorli: ~p, ~p~n", [A, B]),
            error
    end.

% usuwa serwer z danego portu
stop() ->
    try
        ets:delete(clientSet),
        ets:delete(dataSet),
        ets:delete(signalHandlers),
        io:format("Koniec pracy serwera!~n"),
        kontroler_pid:kill(id())
    catch
        _:_ -> io:format("Brak serwera na tym porcie ~p!~n", [port()]),
            error
    end.

% zczytuje dane z serwera a następnie wykonuje odpowiednie operacje w sposob asynchroniczny
nasluchuj() ->
    case klient_UDP:nasluchuj(port()) of
        {error, _} ->
            stop();
        {ClientAddress, _, Data} ->
            spawn(fun () -> wykonaj(ClientAddress, Data) end),
            nasluchuj();
        _ ->
            stop()
    end.

% w zaleznosci od typu danych wykonuje odpowiednie akcje
wykonaj(ClientAddress, {register, Id, ClientPort}) ->
    io:format("Rejestracja ID - ~p.~n", [Id]),
    ets:insert(clientSet, {Id, ClientAddress, ClientPort}),
    io:format("Rejestracja klienta o ID - ~p~n", [Id]);
wykonaj(_, {data, Id, Data}) ->
    ets:insert(dataSet, {Id, Data}),
    io:format("Otrzymywanie danych z ID - ~p: ~p~n", [Id, Data]),
    sygnal(Id);
wykonaj(_, {delete, Id}) ->
    try
        ets:delete(clientSet, Id),
        io:format("Usuwanie klienta o ID -  ~p.~n", [Id])
    catch
        error:badarg -> io:format("Nie istanieje klient o ID - ~p!~n", [Id])
    end.


% zwraca dane pochodzace od klienta o danym ID
retrieveData(Id) ->
    case ets:lookup(dataSet, Id) of
        [] -> nil;
        [{Id, Data}] -> Data
    end.

% wysyła dane do klienta o danym ID
przekazSygnal(Id, Data) ->
    case ets:lookup(clientSet, Id) of
        [] -> nil;
        [{Id, ClientAddress, ClientPort}] ->
            czujniki_UDP:send(ClientAddress, ClientPort, Data)
    end.

% kontroler funkcji biorących udział w przyjmowaniu danych
sygnal(Id) ->
    case ets:lookup(signalHandlers, Id) of
        [] -> nil;
        Funcs ->
            lists:map(fun ({_, Func}) -> Func(retrieveData(Id)) end, Funcs)
    end.


% w zależności od otzymanej temepratury włacza lub wyłącza klimatyzacje
temperatura(nil) -> nil;
temperatura(Data) when Data > 28 ->
    log("Temperatura wynosi : " ++ integer_to_list(Data)),
    przekazSygnal(klima, on),
    przekazSygnal(rolety, on);
temperatura(Data) when Data =< 28  ->
    log("Temperatura wynosi: " ++ integer_to_list(Data)),
    przekazSygnal(klima, off),
    przekazSygnal(rolety, off).


% kontroler drzwi
godzina(nil) -> nil;
godzina(Data) when Data >= 7 ->
    log("Godzina: " ++ integer_to_list(Data)),
    przekazSygnal(drzwi, on);
godzina(Data) when Data < 7 ->
    log ("Godzina: " ++ integer_to_list(Data)),
    przekazSygnal(drzwi, off).


% kontroler czujnika antywlamaniowego
wlamanie(nil) -> nil;
wlamanie(Data) when Data < 7 ->
  log("Godzina: " ++ integer_to_list(Data)),
  log("Czujnik wykryl wlamanie!"),
  przekazSygnal(alarm, "KTOS WLAMAL SIE DO BUDYNKU");
wlamanie(Data) when Data >= 7 ->
  log("Godzina: " ++ integer_to_list(Data)),
  log("Falszywy alarm antywlamaniowy!").

% kontroler detektora dymu
dym(tak) ->
    log("Detektor zidentyfikowal dym!"),
    przekazSygnal(alarm, "Detektor zidentyfikowal dym!"),
    log("Uruchamianie zraszacza przeciwpozarowego!"),
    przekazSygnal(zraszacz, on);
dym(_) ->
    przekazSygnal(zraszacz, off).

% zapisuje logi do pliku tekstowego
log(Line) ->
    io:format("~s~n", [Line]),
    {ok, Log} = file:open("log.txt", [append]),
    io:format(Log, "[~p] ~s~n", [erlang:localtime(), Line]),
    file:close(Log).