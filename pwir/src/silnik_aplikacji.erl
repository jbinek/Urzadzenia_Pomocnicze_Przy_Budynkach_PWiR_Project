-module(silnik_aplikacji).
-export([start/0, stop/0, gui/0]).

% silnik aplikacji

launchTimeInterval() -> 2.
stopTimeInterval() -> 1.

% towrzy wszyztkie potrzebne zasoby i uruchamia aplikację
start() ->



    kontroler_pid:init(),


    %centrum kontroli
    ControllerPID = spawn(fun () -> centrum_kontroli:start() end),
    io:format("Run [controller] process: ~p~n", [ControllerPID]),
    timer:sleep(timer:seconds(launchTimeInterval())),


    %klienci
    AlarmPID = spawn(fun () -> alarm:start() end),
    io:format("Run [Alarm] process: ~p~n", [AlarmPID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    AC_PID = spawn(fun () -> klimatyzacja:start() end),
    io:format("Run [Air Conditioning] process: ~p~n", [AC_PID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    ROLETY_PID = spawn(fun () -> rolety:start() end),
    io:format("Run [rolety] process: ~p~n", [ROLETY_PID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    SprinklerPID = spawn(fun () -> zraszacz_przeciwpozarowy:start() end),
    io:format("Run [Fire Sprinkler] process: ~p~n", [SprinklerPID]),
    timer:sleep(timer:seconds(launchTimeInterval())),




    %urzadzenia
    AI_PID = spawn(fun () -> czujnik_antywlamaniowy:start() end),
    io:format("Run [Anti Intrusion Sensor] process: ~p~n", [AI_PID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    TempPID = spawn(fun () -> miernik_temperatury:start() end),
    io:format("Run [Temperature Sensor] process: ~p~n", [TempPID]),
    timer:sleep(timer:seconds(launchTimeInterval())),

    SmokePID = spawn(fun () -> detektor_dymu:start() end),
    io:format("Run [Smoke Sensor] process: ~p~n", [SmokePID]).

%zatrzymuje działanie aplikacji
stop() ->

    %urządzenia
    detektor_dymu:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    miernik_temperatury:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    czujnik_antywlamaniowy:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    %klienci
    alarm:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    klimatyzacja:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    rolety:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    zraszacz_przeciwpozarowy:stop(),
    timer:sleep(timer:seconds(stopTimeInterval())),

    %centurm kontroli
    centrum_kontroli:stop(),


    kontroler_pid:destroy().


gui() ->
    P_PID = self(),
    Wx=wx:new(),
    Frame=wxFrame:new(Wx, -1, "Urzadzenia pomocnicze przy budynkach GUI"),
    Panel = wxPanel:new(Frame),
    StartButton = wxButton:new(Panel, 12, [{label,"URUCHOM"}, {pos, {50, 50}}]),
    wxButton:connect(StartButton, command_button_clicked, [{callback,
        fun(_, _) -> P_PID ! start end }]),
    StopButton = wxButton:new(Panel, 12, [{label,"STOP"}, {pos, {210, 50}}]),
    wxButton:connect(StopButton, command_button_clicked, [{callback,
        fun(_, _) -> P_PID ! stop end }]),
    wxFrame:show(Frame),

    awaitStart().

%kontroler GUI
awaitStart() ->
    receive
        start -> start()
    end,
    awaitStop().

%kontroler GUI
awaitStop() ->
    receive
        stop -> stop()
    end,
    awaitStart().