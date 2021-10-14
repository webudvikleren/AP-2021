-module(async).
-behaviour(gen_server).


-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).
-export([start/1, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% =============================================================================
%                              API FUNCTIONS
% =============================================================================

new(Fun, Arg) -> gen_server:start_monitor(?MODULE, {Fun, Arg}, []).
wait(Aid) -> gen_server:call(Aid, wait).
poll(Aid) -> gen_server:call(Aid, poll).
wait_catch(Aid) -> gen_Server:call(Aid, wait_catch).
wait_any(Aids) -> lists:foreach(fun(Aid) ->
                                gen_server:call(Aid, wait_any)
                                end, Aids).

% =============================================================================
%                              CALLBACK FUNCTIONS
% =============================================================================


handle_call(wait, _From, State={Working, Succes, Res}) ->
    case {Working, Succes, Res} of
       {true,_,_} -> wait(_From);
       {false,true,Res} -> {reply, Res, State};
       {false,false,Ex} -> {reply, throw(Ex), State}
    end;

handle_call(poll, _From, State={Working, Succes, Res}) ->
    case {Working, Succes, Res} of
       {true,_,_} -> {reply, nothing, State};
       {false,true,Res} -> {reply, {ok, Res}, State};
       {false,false,Ex} -> {reply, {exception, Ex}, State}
    end;

handle_call(wait_catch, _From, State={Working, Succes, Res}) ->
    case {Working, Succes, Res} of
       {true,_,_} -> wait_catch(_From);
       {false,true,Res} -> {reply, {ok, Res}, State};
       {false,false,Ex} -> {reply, {exception, Ex}, State}
    end;

handle_call(wait_any, _From, State={Working, Succes, Res}) ->
    case {Working, Succes, Res} of
       {true,_,_} -> wait(_From);
       {false,true,Res} -> {reply, Res, State};
       {false,false,Ex} -> {reply, throw(Ex), State}
    end;

handle_call({worker,{false, true, Res}}, _From, _) ->
    {reply, Res, {false, true, Res}};

handle_call({worker,{false, false, Reason}}, _From, _) ->
    {reply, Reason, {false, true, Reason}}.

init(_Args={Fun, Arg}) ->
        Me = self(),
        spawn(fun() ->
                   try
                     Res = Fun(Arg),
                     gen_server:call(Me, {worker, {false, true, Res}})
                   catch
                     _:Reason -> gen_server:call(Me, {worker, {false, false, Reason}})
                  end end),
        {ok, {true, false, nothing}}.










% unused right now
start(Name) ->
    sup:start_child(Name).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).


% dummy implementations to satisfy behavior.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.