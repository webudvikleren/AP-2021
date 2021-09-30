-module(async).

-export([new/2, wait/1, poll/1]).

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

wait(Aid) -> 
  Aid ! {self(), get_state},
  receive
    {Working, _, _} ->
      if
        Working -> wait(Aid);
        true -> ok
      end
  end.

poll(Aid) ->
  Aid ! {self(), get_state},
  receive
    {Working, Success, Res} ->
      if
        Working -> nothing;
        Success -> {ok, Res};
        true -> {exception, Res}
      end
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