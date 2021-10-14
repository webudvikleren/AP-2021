-module(async).
-behaviour(gen_server).


-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% =============================================================================
%                              API FUNCTIONS
% =============================================================================

new(Fun, Arg) -> gen_server:start(?MODULE, {Fun, Arg}, []).

wait(Aid) -> case gen_server:call(Aid, call, infinity) of
                  nothing -> wait(Aid);
                  {ok,Res} -> Res;
                  {exception,Ex} -> throw(Ex)
             end.

poll(Aid) -> gen_server:call(Aid, call, infinity).

wait_catch(Aid) -> case gen_server:call(Aid, call, infinity) of
                        nothing -> wait_catch(Aid);
                        {ok, Res} -> {ok, Res};
                        {exception, Ex} -> {exception, Ex}
                   end.

%% wait_any(Aids) that waits for any of the supplied asynchronous actions to complete,
%% where Aids is a non-empty list of asynchronous actions. If the first to complete
%% throws an exception, then that exception is re-thrown by wait_any.
%% Returns a pair {Aid, Res} where Aid is the action ID that completed with the result Res.
wait_any(_Aids) -> nope.

% =============================================================================
%                              CALLBACK FUNCTIONS
% =============================================================================


handle_call(call, _From, State) ->
    case State of
       {true,_,_} -> {reply, nothing, State};
       {false,true,Res} -> {reply, {ok, Res}, State};
       {false,false,Ex} -> {reply, {exception, Ex}, State}
    end;

handle_call({worker,State}, _From, _) ->
    case State of
        {false, true, Res} -> {reply, Res, {false, true, Res}};
        {false, false, Reason} -> {reply, Reason, {false, false, Reason}}
    end.

init(_Args={Fun, Arg}) ->
        Me = self(),
        spawn(fun() ->
                   try
                     Res = Fun(Arg),
                     gen_server:call(Me, {worker, {false, true, Res}})
                   catch
                     _:Reason -> gen_server:call(Me,
                                 {worker, {false, false, Reason}})
                  end end),
        {ok, {true, false, nothing}}.

% dummy implementations to satisfy behavior.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.