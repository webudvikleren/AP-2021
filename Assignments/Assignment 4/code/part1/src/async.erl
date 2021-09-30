-module(async).

-export([new/2, wait/1, poll/1]).

new(Fun, Arg) -> spawn(fun() -> 
    Me = self(),
    process_flag(trap_exit, true),
    spawn_link(fun() ->
      Res = Fun(Arg),
      Me ! {comp_done, Res}
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

loop({Working, Success, Res} = State) ->
  receive
    {comp_done, _Res} -> 
      io:format("Hej verden"),
      loop({false, true, _Res});
    {'EXIT', Worker, Reason} ->
      io:format(Reason),
      if
        Reason /= "normal" ->
          loop({false, false, Reason}),
          io:format("Fejl")
      end;
    {From, get_state} ->
      From ! State,
      loop(State)
  end.