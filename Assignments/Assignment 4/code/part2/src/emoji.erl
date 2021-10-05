-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, dict_delete/2]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

%% checks the initial list for duplicates and starts up the emoji server.
-spec start([{string(), binary()}]) -> any().
start(Initial) ->
  case length(Initial) == sets:size(sets:from_list(Initial)) of
    true -> 
      Pid = spawn(fun() ->
        loop({dict:from_list(Initial), dict:new(),  dict:new()}) 
      end),
      {ok, Pid};
    false -> {error, "Initial elements contains duplicates"}
  end.

%% add new non-existing short codes.
-spec new_shortcode(pid(), _, _) -> any().
new_shortcode(E, Short, Emo) -> 
  Ref = make_ref(),
  Me = self(),
  send_request(E, {Me, Ref, {register, Short, Emo}}),
  receive
    {Ref,RetVal} -> RetVal
  end.

%% Add an alias for a short code.
-spec alias(pid(),_, _) -> any().
alias(E, Short1, Short2) -> 
  Ref = make_ref(),
  Me = self(),
  send_request(E, {Me, Ref, {alias, Short1, Short2}}),
  receive
    {Ref,RetVal} -> RetVal
  end.

%% Delete a short code.
-spec delete(pid(), _) ->
  {pid(), {'remove_analytics', _, _}} |
  {pid(),
   reference(),
   {'delete', _} |
   {'get_analytics', _} |
   {'lookup', _} |
   {'alias', _, _} |
   {'register', _, _} |
   {'register_analytics', _, _, _, _}}.
delete(E, Short) -> Ref = make_ref(),
send_request(E, {self(), Ref, {delete, Short}}).

%% Look up a short code. Subsequently runs all the attached analytics functions.
-spec lookup(pid(), _) -> any().
lookup(E, Short) ->
  Me = self(),
  Ref = make_ref(),
  send_request(E, {Me, Ref, {lookup, Short}}),
  receive
    {Ref,RetVal} -> RetVal
  end.

%% Add an analytics functions to a short code.
- spec analytics(pid(), _, _, _, _) -> any().
analytics(E, Short, Fun, Label, Init) ->
  Ref = make_ref(),
  send_request(E, {self(), Ref, {register_analytics, Short, Fun, Label, Init}}),
  receive
    {Ref,RetVal} -> RetVal
  end.

%% Get the results of the analytics functions of a given short code.
-spec get_analytics(pid(), _) -> any().
get_analytics(E,Short) ->
  Ref = make_ref(),
  send_request(E, {self(), Ref, {get_analytics, Short}}),
  receive
    {Ref,RetVal} -> RetVal
  end.

%% Remove an analytics functions from a short code.
-spec remove_analytics(pid(), _, _) ->
  {pid(), {'remove_analytics', _, _}} |
  {pid(),
   reference(),
   {'delete', _} |
   {'get_analytics', _} |
   {'lookup', _} |
   {'alias', _, _} |
   {'register', _, _} |
   {'register_analytics', _, _, _, _}}.
remove_analytics(E, Short, Label) ->
  Ref = make_ref(),
  send_request(E, {self(), Ref, {remove_analytics, Short, Label}}).

%% Stop the emoji server. Calling the server after this results in a runtime 
%% error.
stop(E) -> 
  exit(E, ok),
  ok.

%% seperation of concerns :-)
handle_request(Request, State) ->
  case Request of
    {register, Short, Emo} -> reg_aux(Short, Emo, State);
    {alias, Short1, Short2} -> alias_aux(Short1, Short2, State);
    {delete, Short} -> delete_aux(Short, State);
    {lookup, Short} -> lookup_aux(Short, State);
    {register_analytics, Short, Fun, Label, Init} ->
                              reg_analytics_aux(Short, Fun, Label, Init, State);
    {get_analytics, Short} -> get_analytics_aux(Short, State);
    {remove_analytics, Short, Label} -> remove_analytics_aux(Short, Label, State)
  end.


%% Main server loop.
loop(State) ->
receive
     {From, Ref, Request} ->
      Me = self(),
      Worker = spawn(fun() ->
        try
          {Res, NewState} = handle_request(Request, State),
          From ! {Ref, Res},
          Me ! {self(), NewState}
        catch
          _:Reason -> Me ! {error, Reason}
        end
      end),
      NewState = receive 
                {Worker, New} -> New;
                {error, Reason} -> From ! {Ref, {error, Reason}}, State
                end,
   loop(NewState)
   end.

%%%% refactored receive functions from the loop %%%%

reg_aux(Short, Emo, State={Shortcodes, Alias, Analytics}) ->
  case dict:is_key(Short, Shortcodes) of
    true -> {{error, "Short already exists"}, State};
    false -> {ok, {dict:append(Short, Emo, Shortcodes), Alias, Analytics}}
  end.

%%TODO: We do not check if the alias already exists.
alias_aux(Short1, Short2, State={Shortcodes, Alias, Analytics}) ->
    case dict:is_key(Short1, Shortcodes) of
      true -> {ok, {Shortcodes, dict:append(Short1, Short2, Alias), Analytics}};
      false -> case dict_search_for_val(Short1, Alias) of
                true -> {ok, {Shortcodes, dict:append(Short1, Short2, Alias), Analytics}};
                false -> {{error, "Short1 not registered"}, State}
               end
     end.

%% This might need a check that the key exists before trying to delete?
delete_aux(Short, {Shortcodes, Alias, Analytics}) ->
  {ok, {dict:erase(dict_search(Short, Alias), Shortcodes), dict_delete(Short, Alias), Analytics}}.

lookup_aux(Short, State={Shortcodes, Alias, Analytics}) ->
  case dict:is_key(Short, Shortcodes) of
    true ->
      case dict:fetch(Short, Shortcodes) of
        [Head | _] -> {{ok, Head}, {Shortcodes, Alias, run_analytics(Short, Analytics)}};
        Head -> {{ok, Head}, {Shortcodes, Alias, run_analytics(Short, Analytics)}}
      end;
    false -> case dict_search(Short, Alias) of
      Short1 ->
        case dict:is_key(Short1, Shortcodes) of
          true -> 
            case dict:fetch(Short1, Shortcodes) of
              [Head | _] -> {{ok, Head}, {Shortcodes, Alias, run_analytics(Short1, Analytics)}};
              Head -> {{ok, Head}, {Shortcodes, Alias, run_analytics(Short1, Analytics)}}
            end;
          false -> {no_emoji, State}
        end
    end
  end.

reg_analytics_aux(Short, Fun, Label, Init, State={Shortcodes, Alias, Analytics}) ->
    case dict:is_key(Short, Shortcodes) or dict:is_key(Short, Alias) or
         dict_search_for_val(Short, Alias) of
        true ->
         ChildShort = dict_search(Short, Alias),
         case dict:is_key(ChildShort, Analytics) of
           true -> Values = lists:nth(1, dict:fetch(ChildShort, Analytics)),
                   case dict:is_key(Label, Values) of
                     true -> {{error, "label already exits."}, State};
                     false -> InnerValues = dict:append(Label, {Fun, Init}, Values),
                              Erase = dict:erase(ChildShort, Analytics),
                              {ok, {Shortcodes, Alias, dict:append(ChildShort, InnerValues, Erase)}}
                   end;
           false -> Values = dict:new(),
           {ok, {Shortcodes, Alias, dict:append(ChildShort, dict:append(Label, {Fun, Init}, Values), Analytics)}}
         end;
        false -> {{error, "shortcode does not exist."}, State}
    end.

get_analytics_aux(Short, State={Shortcodes, Alias, Analytics}) ->
  case dict:is_key(Short, Shortcodes) or dict:is_key(Short, Alias) or
  dict_search_for_val(Short, Alias) of
    true -> ChildShort = dict_search(Short, Alias),
      case dict:is_key(ChildShort, Analytics) of
        true -> 
          Fun_dict = lists:nth(1,dict:fetch(ChildShort, Analytics)),
          Fun_labels = dict:to_list(Fun_dict),
          {{ok, lists:map(fun({Label, [{_, AnalyticsState}]}) ->
          {Label, AnalyticsState}
          end , Fun_labels)}, State};
        false -> 
          {{error, "no analytics registered."}, State}
      end;
    false -> 
      {{error, "shortcode does not exist."}, State}
  end.

%% should perhaps also check if the short code actually exists?
remove_analytics_aux(Short, Label, {Shortcodes, Alias, Analytics}) ->
  {ok,{Shortcodes, Alias, delete_analytics(Short, Label, Analytics)}}.


%%%%% HELPER FUNCTIONS %%%%%

%% Delete analytics functions from a short code and all its' aliases.
delete_analytics(Short, RemoveLabel, Dict) ->
  ChildShort = dict_search(Short, Dict),
  dict:from_list(lists:map(fun({Key, [Functions]}) -> 
    case Key == ChildShort of
      true -> {Key, [dict:erase(RemoveLabel, Functions)]};
      false -> {Key, Functions}
    end
  end, dict:to_list(Dict))).

%% Used by lookup to run the analytics functions (if any exists) attached to
%% the short code.
run_analytics(Short, Dict) ->
  ChildShort = dict_search(Short, Dict),
  dict:from_list(lists:map(fun({Key, Value}) -> 
            case Key == ChildShort of
              true -> Fun_dict = lists:nth(1,dict:fetch(ChildShort, Dict)),
                      Fun_labels = dict:to_list(Fun_dict),
                      {Key, [dict:from_list(lists:map(fun({Label, [{G, State}]}) ->
                      {Label, [{G, G(ChildShort, State)}]}
                      end , Fun_labels))]};
              false -> {Key, Value}
            end
          end, dict:to_list(Dict))).

%% This function is used to the delete the aliases of a short code which
%% deletion has been requested. 
dict_delete(Short, Dict) -> 
  lists:foldl(
    fun(_Short, Acc) ->
      dict:erase(_Short, dict:from_list(lists:map(fun({Key, Values}) ->
        {Key, lists:filter(fun(Value) -> Value /= _Short end, Values)}
      end, dict:to_list(Acc))))
    end,
    Dict,
    find_alias_list(Dict, Short)
  ).

%% Find the "root" of an alias, i.e. the original short code. This is used to
%% save a lot of space when saving e.g. analytics functions. Everything is
%% just stored at the "root" short code, potentially avoiding a lot of
%% duplication.
dict_search(Short, Dict) -> 
  KeysWithShortAsAlias = lists:map(
    fun({Key, _}) -> Key end,
    lists:filter(
      fun({_, Value}) -> lists:member(Short, Value) end,
      dict:to_list(Dict)
    )
  ),
  case length(KeysWithShortAsAlias) of
    0 -> Short;
    _ -> dict_search(lists:nth(1, KeysWithShortAsAlias), Dict)
  end.

find_alias_list(Dict, Short) ->
  ChildShort = dict_search(Short, Dict),
  lists:filter(fun(Value) -> dict_search(Value, Dict) == ChildShort end,
    sets:to_list(sets:from_list(
    lists:concat(lists:map(fun({_, Value}) -> Value end, dict:to_list(Dict)))
  ))) ++ [ChildShort].

%% Because we use dictionaries to store aliases, e.g. [{"alien", "alien1"}],
%% we can have situations where the value of the {key, value} pair is also
%% a key. This functions findes the values so that they can be treates as keys.
dict_search_for_val(Short, Dict) -> 
  Keys = dict:fetch_keys(Dict),
  Mapped = lists:map(fun(Key) ->
    Values = dict:fetch(Key, Dict),
    lists:map(fun(Value) -> 
      if
        Short == Value -> dict_search(Key, Dict);
        true -> false
      end
    end, Values)
  end, Keys),
  Concatted = lists:concat(Mapped),
  Filtered = lists:filter(fun(Elm) -> Elm /= false end, Concatted),
  length(Filtered) /= 0.

%% used to send request to the server. 
send_request(Pid, Message) ->
  case is_process_alive(Pid) of
    true -> Pid ! Message;
    false -> throw("Process not alive")
  end.