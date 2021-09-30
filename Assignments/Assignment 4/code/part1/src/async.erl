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
    loop({true, false, nothing})
  end
).

%% asks for current state and waits untill process is finished.
wait(Aid) -> 
  Aid ! {self(), get_state},
  receive
    {false, false, Reason} -> throw(Reason);
    {false, true, Res} -> Res;
    {true,_,_} -> wait(Aid)
  end.

%% asks for current state and immediately returns.
poll(Aid) ->
  Aid ! {self(), get_state},
  receive
    {true, _, _} -> nothing;
    {false, true, Res} -> {ok, Res};
    {false, false, Reason} -> {exception, Reason}
  end.

loop(State) ->
  receive
    {comp_done, _Res} -> 
      loop({false, true, _Res});
    {comp_error, _Res} ->
      loop({false, false, _Res});
    {From, get_state} ->
      From ! State,
      loop(State)
  end.