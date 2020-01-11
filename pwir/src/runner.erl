-module(runner).
-export([start/0, stop/0, gui/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Runner
%% Runs the whole application.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

launchTimeInterval() -> 2.
stopTimeInterval() -> 1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: start
%% Purpose: Creates all necessary processes and launches application. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> 

    % Process Manager

    process_manager:init(),

    % Controller

    ControllerPID = spawn(fun () -> controller:start() end),
    io:format("Run [controller] process: ~p~n", [ControllerPID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    % Signal consumers

    AlarmPID = spawn(fun () -> alarm:start() end),
    io:format("Run [Alarm] process: ~p~n", [AlarmPID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    AC_PID = spawn(fun () -> air_conditioning:start() end),
    io:format("Run [Air Conditioning] process: ~p~n", [AC_PID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    SprinklerPID = spawn(fun () -> fire_sprinkler:start() end),
    io:format("Run [Fire Sprinkler] process: ~p~n", [SprinklerPID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    

    % Signal emitters

    AI_PID = spawn(fun () -> anti_intrusion_sensor:start() end),
    io:format("Run [Anti Intrusion Sensor] process: ~p~n", [AI_PID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    TempPID = spawn(fun () -> temperature_sensor:start() end),
    io:format("Run [Temperature Sensor] process: ~p~n", [TempPID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    SmokePID = spawn(fun () -> smoke_sensor:start() end),
    io:format("Run [Smoke Sensor] process: ~p~n", [SmokePID]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: stop
%% Purpose: Stops the application.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop() ->

    % Signal emitters

    smoke_sensor:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    temperature_sensor:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    anti_intrusion_sensor:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    % Signal consumers

    alarm:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    air_conditioning:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    fire_sprinkler:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    % Controller

    controller:stop(),

    % Process Manager

    process_manager:destroy().


gui() -> 
    P_PID = self(),
    Wx=wx:new(),
    Frame=wxFrame:new(Wx, -1, "iHome GUI"),
    Panel = wxPanel:new(Frame),
    StartButton = wxButton:new(Panel, 12, [{label,"START"}]),
    wxButton:connect(StartButton, command_button_clicked, [{callback, 
        fun(_, _) -> P_PID ! start end }]),
    StopButton = wxButton:new(Panel, 12, [{label,"STOP"}, {pos, {50, 50}}]),
    wxButton:connect(StopButton, command_button_clicked, [{callback, 
        fun(_, _) -> P_PID ! stop end }]),
    wxFrame:show(Frame),

    awaitStart().


awaitStart() -> 
    receive 
        start -> start()
    end,
    awaitStop().

awaitStop() ->
    receive
        stop -> stop()
    end,
    awaitStart().