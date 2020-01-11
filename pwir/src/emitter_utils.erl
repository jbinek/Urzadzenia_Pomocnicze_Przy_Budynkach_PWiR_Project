-module(emitter_utils).
-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpful functions that implement basic interactions between client and the server.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: register
%% Purpose: Registers client on server of given address and port. Client is being registered
%% with given ID, name and receiving port.
%% Arguments: Server's address and port, client's ID, name and receiving port.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register(ServerAddress, ServerPort, Id, ClientPort) -> 
    send(ServerAddress, ServerPort, Id, {register, Id, ClientPort}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: data
%% Purpose: Sends data on server's given address and port from client of given ID.
%% Arguments: Server's address and port, client's ID and data.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sendData(ServerAddress, ServerPort, Id, Data) -> 
    send(ServerAddress, ServerPort, Id, {data, Id, Data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: delete
%% Purpose: Deletes client of a given ID from the server of a given address and port.
%% Arguments: Server's address and port, client's ID.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unregister(ServerAddress, ServerPort, Id) -> 
    send(ServerAddress, ServerPort, Id, {delete, Id}).

send(ServerAddress, ServerPort, OperationId, Payload) -> 
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    io:format("~p : -> ~p:~p [~p]~n", [OperationId, ServerAddress, ServerPort, Payload]),
    gen_udp:send(Socket, ServerAddress, ServerPort, term_to_binary(Payload)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: send
%% Purpose: Sends data using UDP protocol on given address and port.
%% Arguments: Address, port and data to send.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send(Address, Port, Data) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    gen_udp:send(Socket, Address, Port, term_to_binary(Data)).

    
