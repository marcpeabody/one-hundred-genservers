%% erlang
% Taken from AtomVM https://www.atomvm.net/sample-code/

-module(blinky2).

-export([start/0]).

start() ->
    gpio:set_pin_mode(2, output),
    gpio:set_pin_mode(4, output),
    spawn(fun() -> toggle(2, 1000, low) end),
    spawn(fun() -> toggle(4, 500, low) end),
    timer:sleep(infinity).

toggle(Pin, SleepMs, low) ->
    gpio:digital_write(Pin, low),
    timer:sleep(SleepMs),
    toggle(Pin, SleepMs, high);
toggle(Pin, SleepMs, high) ->
    gpio:digital_write(Pin, high),
    timer:sleep(SleepMs),
    toggle(Pin, SleepMs, low).
