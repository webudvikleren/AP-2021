-module(async).

-export([new/2, wait/1, poll/1]).

%% Spawns a supervisor and a worker process. The worker process handles the
%% function evaluation. The supervisor calls the loop.
new(Fun, Arg) -> spawn(fun() -> 
    Me = self(),
    spawn(fun() ->
      try
        Res = Fun(Arg),
        Me ! {comp_done, Res}
      catch
        _:Reason -> Me ! {comp_error, Reason}
      end
    end),
    loop({working, noSucces, nothing})
  end
).

%% asks for current state and waits untill process is finished.
wait(Aid) -> 
  Aid ! {self(), get_state},
  receive
    {notWorking, noSucces, Reason} -> throw(Reason);
    {notWorking, succes, Res} -> Res;
    {working,_,_} -> wait(Aid)
  end.

%% asks for current state and immediately returns.
poll(Aid) ->
  Aid ! {self(), get_state},
  receive
    {working, _, _} -> nothing;
    {notWorking, succes, Res} -> {ok, Res};
    {notWorking, noSucces, Ex} -> {exception, Ex}
  end.

loop(State) ->
  receive
    {comp_done, _Res} -> 
      loop({notWorking, succes, _Res});
    {comp_error, Ex} ->
      loop({notWorking, noSucces, Ex});
    {From, get_state} ->
      From ! State,
      loop(State)
  end.