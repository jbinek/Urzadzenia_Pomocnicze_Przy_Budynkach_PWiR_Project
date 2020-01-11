-module(controller).
-export([start/0, stop/0, port/0, address/0, handleTemperature/1, handleSmoke/1, hanleIntrusion/1]).

% Główny server aplikacji - wymienia dane między klientami i wywołuje funkcje w oparciu o otrzymane dane

port() -> 5000.
address() -> {127,0,0,1}.
id() -> controller.

% START
% Uruchamia server na podanym porcie i tworzy potrzebne kontrolery danych

start() ->
    try
        ets:new(clientSet, [set, named_table, public]),
        ets:new(dataSet, [set, named_table, public]),
        ets:new(signalHandlers, [bag, named_table, public]),
        io:format("Serwer na porcie ~p uruchamia się...~n", [port()]),

        ets:insert(signalHandlers, {temp, fun handleTemperature/1}),
        ets:insert(signalHandlers, {smoke, fun handleSmoke/1}),
        ets:insert(signalHandlers, {intrusion, fun hanleIntrusion/1}),

        process_manager:register(id(), self()),
        listen(),
        start
    catch
        A:B -> io:format("Error podczas uruchamiania procesu: ~p, ~p~n", [A, B]),
        error
    end.

% STOP
% Zatrzymuje serwer i usuwa wszystkie istniejące kontenery danych

stop() ->
    try
        ets:delete(clientSet),
        ets:delete(dataSet),
        ets:delete(signalHandlers),
        io:format("Server stopped!~n"),
        process_manager:kill(id())
    catch
        _:_ -> io:format("No working server on port ~p!~n", [port()]),
        error
    end.

% LISTEN
% Nasłuchuje i wywołuje asynchronicznie funkcje

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

% ACT
% Reaguje na otrzymane dane

act(ClientAddress, {register, Id, ClientPort}) ->
    ets:insert(clientSet, {Id, ClientAddress, ClientPort}),
    io:format("Rejestracja klient o ID = ~p~n", [Id]);
act(_, {data, Id, Data}) ->
    ets:insert(dataSet, {Id, Data}),
    io:format("Otrzymano dane od klienta o ID = ~p: ~p~n", [Id, Data]),
    handleSignal(Id);
act(_, {delete, Id}) ->
    try
        ets:delete(clientSet, Id),
        io:format("Zatrzymywanie klienta o ID = ~p.~n", [Id])
    catch
        error:badarg -> io:format("Brak klienta o ID = ~p!~n", [Id])
    end.


% RETRIEVEDATA
% Zwraca dane od klienta

retrieveData(Id) ->
    case ets:lookup(dataSet, Id) of
        [] -> nil;
        [{Id, Data}] -> Data
    end.

% FORWARDSIGNAL
% Wysyła dane do klienta

forwardSignal(Id, Data) ->
    case ets:lookup(clientSet, Id) of
        [] -> nil;
        [{Id, ClientAddress, ClientPort}] ->
            emitter_utils:send(ClientAddress, ClientPort, Data)
    end.

% HANDLESIGNAL
% Wywołuje funkcje w zależności od otrzymanych danych

handleSignal(Id) ->
    case ets:lookup(signalHandlers, Id) of
        [] -> nil;
        Funcs ->
                lists:map(fun ({_, Func}) -> Func(retrieveData(Id)) end, Funcs)
    end.


% HANDLETEMPERATURE
% Reaguje na temperaturę
handleTemperature(nil) -> nil;
handleTemperature(Data) when Data > 28 ->
    log("Klimatyzacja WŁĄCZONA, temperatura: " ++ integer_to_list(Data)),
    forwardSignal(ac, on);
handleTemperature(_)  ->
    log("Temperatura w porządku, wyłączanie klimatyzacji"),
    forwardSignal(ac, off).


% HANDLEINTRUSION
% Reaguje na czujnik antywłamaniony

hanleIntrusion(yes) ->
    log("KTOś WŁAMUJE SIE DO BUDYNKU!"),
    forwardSignal(alarm, "KTOś WŁAMUJE SIE DO BUDYNKU!");
hanleIntrusion(_) -> nil.

% HANDLESMOKE
% Reaguje na czujnik dymu

handleSmoke(yes) ->
    log("Czujnik wykrył dym!"),
    forwardSignal(alarm, "Czujnik wykrył dym!"),
    log("Włączanie zraszacza..."),
    forwardSignal(sprinkler, on);
handleSmoke(_) ->
    forwardSignal(sprinkler, off).

% LOG
% Zapisuje dane do logów

log(Line) -> 
    io:format("~s~n", [Line]),
    {ok, Log} = file:open("log.txt", [append]),
    io:format(Log, "[~p] ~s~n", [erlang:localtime(), Line]),
    file:close(Log).
