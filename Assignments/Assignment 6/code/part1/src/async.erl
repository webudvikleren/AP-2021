-module(async).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).
-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).
-export([working/3, notWorking/3]).

% =============================================================================
%                              API FUNCTIONS
% =============================================================================

new(Fun, Arg) -> {ok, Aid} = gen_statem:start(?MODULE, {Fun, Arg}, []),
                 Aid.

poll(Aid) -> gen_statem:call(Aid, poll).

wait(Aid) -> case gen_statem:call(Aid, wait) of
                {ok, Res} -> Res;
                {exception, Ex} -> throw(Ex)
             end.
            
wait_catch(Aid) -> case gen_statem:call(Aid, wait) of
                        {ok, Res} -> {ok, Res};
                        {exception, Ex} -> {exception, Ex}
                    end.

wait_any(Aids) ->
    Ref = make_ref(),
    Me = self(),
    spawn(fun() -> Sup = self(),
    lists:foreach(fun(Aid) -> spawn(fun() ->
                                    Res = gen_statem:call(Aid, wait),
                                    Sup ! {Aid, Ref, Res}
                                    end) end, Aids),
    receive
       {Aid, Ref, Res} -> Me ! {Aid, Ref, Res}, exit(normal)
    end end),

    receive
        {Aid, Ref, {ok, Res}} -> {Aid, Res};
        {_, Ref, {exception, Ex}} -> throw({exception,Ex})
    end.

% =============================================================================
%                              CALLBACK FUNCTIONS
% =============================================================================

%% The state can only be changed by the worker. From working to notWorking.
working(cast, {worker, EvalRes}, _Data) ->
    {next_state, notWorking, EvalRes};

working({call, From}, poll, _Data) ->
    {keep_state_and_data, [{reply, From, nothing}]};

working({call, _From}, wait, _Data) ->
        {keep_state_and_data, [postpone]}.

notWorking({call, From}, poll, Data) ->
        {keep_state_and_data, [{reply, From, Data}]};

notWorking({call, From}, wait, Data) ->
        {keep_state_and_data, [{reply, From, Data}]}.

init(_Args={Fun, Arg}) ->
    Me = self(),
    spawn(fun() ->
               try
                 Res = Fun(Arg),
                 gen_statem:cast(Me, {worker, {ok, Res}})
               catch
                 _:Reason -> gen_statem:cast(Me, {worker, {exception, Reason}})
              end end),
    Data = {ok, noRes},
    {ok, working, Data}.

callback_mode() ->
    state_functions.

%% The last 4 functions are interface functions to satisfy behaviour.
handle_event(enter, _OldState, _State, _Data) ->
    keep_state_and_data;

handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.