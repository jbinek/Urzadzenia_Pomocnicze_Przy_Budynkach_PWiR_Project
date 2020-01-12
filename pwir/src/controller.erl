-module(controller).
-export([start/0, stop/0, port/0, address/0, handleTemperature/1, handleSmoke/1, hanleIntrusion/1]).
% serwer aplikacji, centrum kontroli

port() -> 5000.
address() -> {127,0,0,1}.
id() -> controller.

%uruchamia serwer na porcie 5000
start() ->
    try
        ets:new(clientSet, [set, named_table, public]),
        ets:new(dataSet, [set, named_table, public]),
        ets:new(signalHandlers, [bag, named_table, public]),
        io:format("Uruchomienie serwera na porcie ~p ~n", [port()]),

        ets:insert(signalHandlers, {temp, fun handleTemperature/1}),
        ets:insert(signalHandlers, {smoke, fun handleSmoke/1}),
        ets:insert(signalHandlers, {intrusion, fun hanleIntrusion/1}),

        process_manager:register(id(), self()),
        listen(),
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
        process_manager:kill(id())
    catch
        _:_ -> io:format("Brak serwera na tym porcie ~p!~n", [port()]),
            error
    end.

% zczytuje dane z serwera a następnie wykonuje odpowiednie operacje w sposob asynchroniczny
listen() ->
    case consumer_utils:listen(port()) of
        {error, _} ->
            stop();
        {ClientAddress, _, Data} ->
            spawn(fun () -> act(ClientAddress, Data) end),
            listen();
        _ ->
            stop()
    end.

% w zaleznosci od typu danych wykonuje odpowiednie akcje
act(ClientAddress, {register, Id, ClientPort}) ->
    io:format("Rejestracja ID - ~p.~n", [Id]),
    ets:insert(clientSet, {Id, ClientAddress, ClientPort}),
    io:format("Rejestracja klienta o ID - ~p~n", [Id]);
act(_, {data, Id, Data}) ->
    ets:insert(dataSet, {Id, Data}),
    io:format("Otrzymywanie danych z ID - ~p: ~p~n", [Id, Data]),
    handleSignal(Id);
act(_, {delete, Id}) ->
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
forwardSignal(Id, Data) ->
    case ets:lookup(clientSet, Id) of
        [] -> nil;
        [{Id, ClientAddress, ClientPort}] ->
            emitter_utils:send(ClientAddress, ClientPort, Data)
    end.

% kontroler funkcji biorących udział w przyjmowaniu danych
handleSignal(Id) ->
    case ets:lookup(signalHandlers, Id) of
        [] -> nil;
        Funcs ->
            lists:map(fun ({_, Func}) -> Func(retrieveData(Id)) end, Funcs)
    end.


% w zależności od otzymanej temepratury włacza lub wyłącza klimatyzacje
handleTemperature(nil) -> nil;
handleTemperature(Data) when Data > 28 ->
    log("Klimatyzacja wlaczona, temperatura wynosi : " ++ integer_to_list(Data)),
    forwardSignal(ac, on);
handleTemperature(Data) when Data =< 28  ->
    log("Klimatyzacja wylaczona, temperatura wynosi: " ++ integer_to_list(Data)),
    forwardSignal(ac, off).


% kontroler czujnika antywlamaniowego
hanleIntrusion(yes) ->
    log("Ktos wlamal sie do budynku!"),
    forwardSignal(alarm, "Ktos wlamal sie do budynku!");
hanleIntrusion(_) -> nil.

% kontroler detektora dymu
handleSmoke(yes) ->
    log("Detektor zidentyfikowal dym!"),
    forwardSignal(alarm, "Detektor zidentyfikowal dym!"),
    log("Uruchamianie zraszacza przeciwpozarowego!"),
    forwardSignal(sprinkler, on);
handleSmoke(_) ->
    forwardSignal(sprinkler, off).

% zapisuje logi do pliku tekstowego
log(Line) ->
    io:format("~s~n", [Line]),
    {ok, Log} = file:open("log.txt", [append]),
    io:format(Log, "[~p] ~s~n", [erlang:localtime(), Line]),
    file:close(Log).