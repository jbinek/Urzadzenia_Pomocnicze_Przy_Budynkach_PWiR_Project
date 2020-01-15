-module(centrum_kontroli).
-export([start/0, stop/0, port/0, address/0, temperatura/1, dym/1, godzina/1,realneWlamanie/1, falszyweWlamanie/1,  drzwiOtwarte/1, drzwiZamkniete/1, wlamanie/1]).
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
    przekazSygnal(alarm, "Temperatura jest za wysoka - aktywacja klimatyzacji i rolet!"),
    przekazSygnal(klima, on),
    przekazSygnal(rolety, on);
temperatura(Data) when Data =< 28  ->
    log("Temperatura wynosi: " ++ integer_to_list(Data)),
    przekazSygnal(alarm, "Temperatura jest optymalna - dezaktywacja klimatyzaji i rolet!"),
    przekazSygnal(klima, off),
    przekazSygnal(rolety, off).

% funkcja pomocnicza do kontrolera dzrwi, tworzy log z godzina i wyswietla okienko z powiadomieniem o otwartych drzwiach
drzwiOtwarte(Data) ->
    log("Godzina: " ++ integer_to_list(Data)),
    przekazSygnal(alarm, "Drzwi są otwarte, godzina - " ++ integer_to_list(Data)),
    przekazSygnal(drzwi, on).

% funkcja pomocnicza do kontrolera dzrwi, tworzy log z godzina i wyswietla okienko z powiadomieniem o zamknietych drzwiach
drzwiZamkniete(Data) ->
    log ("Godzina: " ++ integer_to_list(Data)),
    przekazSygnal(alarm, "Drzwi są zamknięte, godzina - " ++ integer_to_list(Data)),
    przekazSygnal(drzwi, off).


% kontroler drzwi
godzina(nil) -> nil;
godzina(Data) ->
    case Data of
        0 -> drzwiZamkniete(Data);
        1 -> drzwiZamkniete(Data);
        2 -> drzwiZamkniete(Data);
        3 -> drzwiZamkniete(Data);
        4 -> drzwiZamkniete(Data);
        5 -> drzwiZamkniete(Data);
        6 -> drzwiZamkniete(Data);
        7 -> drzwiOtwarte(Data);
        8 -> drzwiOtwarte(Data);
        9 -> drzwiOtwarte(Data);
        10 -> drzwiOtwarte(Data);
        11 -> drzwiOtwarte(Data);
        12 -> drzwiOtwarte(Data);
        13 -> drzwiOtwarte(Data);
        14 -> drzwiOtwarte(Data);
        15 -> drzwiOtwarte(Data);
        16 -> drzwiOtwarte(Data);
        17 -> drzwiOtwarte(Data);
        18 -> drzwiZamkniete(Data);
        19 -> drzwiZamkniete(Data);
        20 -> drzwiZamkniete(Data);
        21 -> drzwiZamkniete(Data);
        22 -> drzwiZamkniete(Data);
        23 -> drzwiZamkniete(Data);
        24 -> drzwiZamkniete(Data)
    end.


% funkcja pomocnicza do czujnika antywlamaniowego, tworzy log z godzina
realneWlamanie(Data) ->
    log("Godzina: " ++ integer_to_list(Data)),
    log("Czujnik wykryl wlamanie!"),
    przekazSygnal(alarm, "KTOS WLAMAL SIE DO BUDYNKU").

% funkcja pomocnicza do czujnika antywlamaniowego, tworzy log z godzina
falszyweWlamanie(Data) ->
    log("Godzina: " ++ integer_to_list(Data)),
    log("Falszywy alarm antywlamaniowy!").


% kontroler czujnika antywlamaniowego
wlamanie(nil) -> nil;
wlamanie(Data)  ->
    case Data of
        0 -> realneWlamanie(Data);
        1 -> realneWlamanie(Data);
        2 -> realneWlamanie(Data);
        3 -> realneWlamanie(Data);
        4 -> realneWlamanie(Data);
        5 -> realneWlamanie(Data);
        6 -> realneWlamanie(Data);
        7 -> falszyweWlamanie(Data);
        8 -> falszyweWlamanie(Data);
        9 -> falszyweWlamanie(Data);
        10 -> falszyweWlamanie(Data);
        11 -> falszyweWlamanie(Data);
        12 -> falszyweWlamanie(Data);
        13 -> falszyweWlamanie(Data);
        14 -> falszyweWlamanie(Data);
        15 -> falszyweWlamanie(Data);
        16 -> falszyweWlamanie(Data);
        17 -> falszyweWlamanie(Data);
        18 -> realneWlamanie(Data);
        19 -> realneWlamanie(Data);
        20 -> realneWlamanie(Data);
        21 -> realneWlamanie(Data);
        22 -> realneWlamanie(Data);
        23 -> realneWlamanie(Data);
        24 -> realneWlamanie(Data)
    end.

% kontroler miernika temperatury
%temperaturaPowiadomienie(nil) -> nil;
%temperaturaPowiadomienie(Data) when Data < 28 ->
%    przekazSygnal(alarm, "Temperatura jest za wysoka - aktywacja klimatyzacji i rolet!");
%temperaturaPowiadomienie(Data) when Data >= 28 ->
%    przekazSygnal(alarm, "Temperatura jest optymalna - dezaktywacja klimatyzaji i rolet!").

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