-module(czujniki_UDP).
-compile(export_all).
% odpowiada za komunikacje między klientem a serwerem

%rejestruje klienta na podanym serwerze
register(ServerAddress, ServerPort, Id, ClientPort) ->
    send(ServerAddress, ServerPort, Id, {register, Id, ClientPort}).

% przesyła dane na serwer od klienta
sendData(ServerAddress, ServerPort, Id, Data) ->
    send(ServerAddress, ServerPort, Id, {data, Id, Data}).

% usuwa klienta z serwera
unregister(ServerAddress, ServerPort, Id) ->
    send(ServerAddress, ServerPort, Id, {delete, Id}).

send(ServerAddress, ServerPort, OperationId, Payload) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    io:format("~p : -> ~p:~p [~p]~n", [OperationId, ServerAddress, ServerPort, Payload]),
    gen_udp:send(Socket, ServerAddress, ServerPort, term_to_binary(Payload)).

% za pomocą UDP przesyła dane na serwer
send(Address, Port, Data) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    gen_udp:send(Socket, Address, Port, term_to_binary(Data)).

    
