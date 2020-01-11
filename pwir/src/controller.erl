-module(controller).
-export([start/0, stop/0, port/0, address/0, handleTemperature/1, handleSmoke/1, hanleIntrusion/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Main server of the application.
%% Exchanges data between clients and 
%% calls functions based on data received
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

port() -> 5000.
address() -> {127,0,0,1}.
id() -> controller.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: start
%% Purpose: Launches server on a given port and creates all necessary data containers. 
%% If given port is already taken error is returned.
%% Arguments: Port.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    try
        ets:new(clientSet, [set, named_table, public]),
        ets:new(dataSet, [set, named_table, public]),
        ets:new(signalHandlers, [bag, named_table, public]),
        io:format("Starting server on port ~p...~n", [port()]),

        ets:insert(signalHandlers, {temp, fun handleTemperature/1}),
        ets:insert(signalHandlers, {smoke, fun handleSmoke/1}),
        ets:insert(signalHandlers, {intrusion, fun hanleIntrusion/1}),

        process_manager:register(id(), self()),
        listen(),
        start
    catch
        A:B -> io:format("Error while running controller process: ~p, ~p~n", [A, B]),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: stop
%% Purpose: Deletes server on a given port and deletes all existing data containers. 
%% If given port is already free error is returned.
%% Arguments: Port.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: listen
%% Purpose: Reads data coming from the server and then reacts asynchronously 
%% based on data received.
%% Arguments: Port.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: act
%% Purpose: Reacts on received data based on their format.
%% Arguments: Client's address, tuple with data.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
act(ClientAddress, {register, Id, ClientPort}) ->
    io:format("register o id ~p.~n", [Id]),
    ets:insert(clientSet, {Id, ClientAddress, ClientPort}),
    io:format("Registering client with id ~p~n", [Id]);
act(_, {data, Id, Data}) ->
    ets:insert(dataSet, {Id, Data}),
    io:format("Received data from ID ~p: ~p~n", [Id, Data]),
    handleSignal(Id);
act(_, {delete, Id}) ->
    try
        ets:delete(clientSet, Id),
        io:format("Deleting client with id ~p.~n", [Id])
    catch
        error:badarg -> io:format("No working client with id ~p!~n", [Id])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: retrieveData
%% Purpose: Returns data, which came from client of given ID.
%% Arguments: client's ID.
%% Returns: Saved data or nil, of no data has been received from client of given ID.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
retrieveData(Id) ->
    case ets:lookup(dataSet, Id) of
        [] -> nil;
        [{Id, Data}] -> Data
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: forwardSignal
%% Purpose: Sends data to client of given ID.
%% Arguments: client's ID, data to send.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
forwardSignal(Id, Data) ->
    case ets:lookup(clientSet, Id) of
        [] -> nil;
        [{Id, ClientAddress, ClientPort}] ->
            emitter_utils:send(ClientAddress, ClientPort, Data)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: exec_func
%% Purpose: Calls all functions that react on receiving data from client of given ID.
%% Arguments: client's ID
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handleSignal(Id) ->
    case ets:lookup(signalHandlers, Id) of
        [] -> nil;
        Funcs ->
                lists:map(fun ({_, Func}) -> Func(retrieveData(Id)) end, Funcs)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: temp
%% Purpose: Turns ac on or off based on temperature received.
%% Arguments: Temperature value.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handleTemperature(nil) -> nil;
handleTemperature(Data) when Data > 28 ->
    log("Air Conditioning ON, temperature : " ++ integer_to_list(Data)),
    forwardSignal(ac, on);
handleTemperature(_)  ->
    log("Temperature is fine, Air Conditioning OFF"),
    forwardSignal(ac, off).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: alarm
%% Purpose: Reacts on anti intrusion sensor. 
%% If intrusion got detected alarm is being activated.
%% Arguments: Alarm state.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hanleIntrusion(yes) ->
    log("Someone is breaking into the house, alarm!"),
    forwardSignal(alarm, "Someone is breaking into the house, alarm!");
hanleIntrusion(_) -> nil.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: alarm/1
%% Purpose: Reacts on smoke sensor state. 
%% If sensor was activated fire sprinkler is being turned on. 
%% Arguments: Smoke sensor state.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handleSmoke(yes) ->
    log("The sensor has detected smoke!"),
    forwardSignal(alarm, "The sensor has detected smoke!"),
    log("Fire sprinkler is being turned on..."),
    forwardSignal(sprinkler, on);
handleSmoke(_) ->
    forwardSignal(sprinkler, off).


log(Line) -> 
    io:format("~s~n", [Line]),
    {ok, Log} = file:open("log.txt", [append]),
    io:format(Log, "[~p] ~s~n", [erlang:localtime(), Line]),
    file:close(Log).
