-module(async).
-behaviour(gen_server).


-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).
-export([start/1, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%not needed?
-record(state, {working, succes, res}).



% =============================================================================
%                              API FUNCTIONS
% =============================================================================

new(Fun, Arg) -> gen_server:start_monitor(?MODULE, {Fun, Arg}, []).
wait(Aid) -> gen_server:call(Aid, wait).
poll(Aid) -> gen_server:call(Aid, poll).
wait_catch(Aid) -> gen_Server:call(Aid, wait_catch).
wait_any(Aids) -> gen_server:call(Aids, wait_any).

% =============================================================================
%                              CALLBACK FUNCTIONS
% =============================================================================


handle_call(poll, _From, State={Working, Succes, Res}) ->
    case {Working, Succes, Res} of
       {working,_,_} -> {reply, nothing, State};
       {notWorking,succes,Res} -> {reply, {ok, Res}, State};
       {notWorking,succes,Ex} -> {reply, {exception, Ex}, State}
    end.




start(Name) ->
    sup:start_child(Name).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% dummy implementation to satisfy behavior.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.