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

%% Wait any with spawn. Do not know how to get the caught value out of lists:foreach
%% it just return "ok.".
wait_any(Aids) -> lists:foreach(fun(Aid) -> spawn(fun() ->
                        catch case gen_server:call(Aid, call, infinity) of
                            nothing -> wait_any([Aid]);
                            {ok, Res} -> io:format("Res"), throw({Aid, Res});
                            {exception, Ex} -> io:format("Ex"), throw(Ex)
                         end
                         end) end, Aids).

%% The return value here is correct, but it waits for all processes to finish,
%% before it returns which is wrong. Maybe spawn a process for each Aid?
%% throw(something), does not throw an exception, just local return which we can
%% catch.
wait_any_(Aids) -> catch lists:foreach(fun(Aid) ->
                         case gen_server:call(Aid, call, infinity) of
                            nothing -> wait_any([Aid]);
                            {ok, Res} -> throw({Aid, Res});
                            {exception, Ex} -> throw(Ex)
                         end
                         end, Aids).

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

% dummy implementations to satisfy behaviour.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.