-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, dict_search/2]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

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

-spec new_shortcode(string(), string(), binary()) -> any().
new_shortcode(E, Short, Emo) -> 
  Me = self(),
  send_request(E, {register, Me, Short, Emo}),
  receive
    RetVal -> RetVal
  end.

-spec alias(string(), string(), string()) -> any().
alias(E, Short1, Short2) -> 
  Me = self(),
  send_request(E, {alias, Me, Short1, Short2}),
  receive
    RetVal -> RetVal
  end.

-spec delete(string(), string()) -> any().
delete(E, Short) -> E ! {delete, Short}.

-spec lookup(string(), string()) -> any().
lookup(E, Short) ->
  Me = self(),
  send_request(E, {lookup, Me, Short}),
  receive
    RetVal -> RetVal
  end.

- spec analytics(string(), shortcode(), analytic_fun(any()), string(), any()) -> any().
analytics(E, Short, Fun, Label, Init) ->
  send_request(E, {register_analytics, self(), Short, Fun, Label, Init}),
  receive
    RetVal -> RetVal
  end.

-spec get_analytics(string(), shortcode()) -> any().
get_analytics(E,Short) ->
  send_request(E, {get_analytics, self(), Short}),
  receive
    RetVal -> RetVal
  end.

-spec remove_analytics(string(), shortcode(), string()) -> any().
remove_analytics(E, Short, Label) ->
  send_request(E, {remove_analytics, Short, Label}).

stop(E) -> 
  exit(E, ok),
  ok.

loop({Shortcodes, Alias, Analytics} = State) ->
receive
  {register, From, Short, Emo} ->
    case dict:is_key(Short, Shortcodes) of
      true -> From ! {error, "Short already exists"}, loop(State);
      false -> From ! ok, loop({dict:append(Short, Emo, Shortcodes), Alias, Analytics})
    end;
  {alias, From, Short1, Short2} ->
    case dict:is_key(Short1, Shortcodes) of
      true -> From ! ok, loop({Shortcodes, dict:append(Short1, Short2, Alias), Analytics});
      false -> case dict_search_for_val(Short1, Alias) of
        true -> From ! ok, loop({Shortcodes, dict:append(Short1, Short2, Alias), Analytics});
        false -> From ! {error, "Short1 not registered"}, loop(State)
      end
    end;
  {delete, Short} ->
    loop({dict:erase(Short, Shortcodes), dict_delete(Short, Alias), Analytics});
  {lookup, From, Short} ->
    case dict:is_key(Short, Shortcodes) of
      true -> 
        Emo = dict:fetch(Short, Shortcodes),
        From ! {ok, Emo},
        loop({Shortcodes, Alias, run_analytics(Short, Analytics)});
      false -> case dict_search(Short, Alias) of
        Short1 ->
          case dict:is_key(Short1, Shortcodes) of
            true -> 
              Emo = dict:fetch(Short1, Shortcodes),
              From ! {ok, Emo},
              loop({Shortcodes, Alias, run_analytics(Short1, Analytics)});
            false -> From ! no_emoji,
                    loop(State)
          end
      end
    end;
  {register_analytics, From, Short, Fun, Label, Init} ->
    case dict:is_key(Short, Shortcodes) or dict:is_key(Short, Alias) or
         dict_search_for_val(Short, Alias) of
        true ->
         ChildShort = dict_search(Short, Alias),
         case dict:is_key(ChildShort, Analytics) of
           true -> Values = lists:nth(1, dict:fetch(ChildShort, Analytics)),
                   case dict:is_key(Label, Values) of
                     true -> From ! {error, "label already exits."}, loop(State);
                     false -> From ! ok,
                              InnerValues = dict:append(Label, {Fun, Init}, Values),
                              Erase = dict:erase(ChildShort, Analytics),
                              loop({Shortcodes, Alias, dict:append(ChildShort, InnerValues, Erase)})
                   end;
           false -> From ! ok,
           Values = dict:new(),
           loop({Shortcodes, Alias, dict:append(ChildShort, dict:append(Label, {Fun, Init}, Values), Analytics)})
         end;
        false -> From ! {error, "shortcode does not exist."}, loop(State)
    end;
  {get_analytics, From, Short} ->
    case dict:is_key(Short, Shortcodes) or dict:is_key(Short, Alias) or
    dict_search_for_val(Short, Alias) of
               true -> ChildShort = dict_search(Short, Alias),
                    case dict:is_key(ChildShort, Analytics) of
                       true -> Fun_dict = lists:nth(1,dict:fetch(Short, Analytics)),
                               Fun_labels = dict:to_list(Fun_dict),
                               From ! {ok, lists:map(fun({Label, [{_, AnalyticsState}]}) ->
                                {Label, AnalyticsState}
                               end , Fun_labels)};
                       false -> From ! {error, "no analytics registered."}
                    end;
               false -> From ! {error, "shortcode does not exist."}
    end,
    loop(State);
  {remove_analytics, Short, Label} ->
       loop({Shortcodes, Alias, delete_analytics(Short, Label, Analytics)})
   end.

delete_analytics(Short, RemoveLabel, Dict) ->
  ChildShort = dict_search(Short, Dict),
  dict:from_list(lists:map(fun({Key, Value}) -> 
           case Key == ChildShort of
             true -> Fun_dict = lists:nth(1,dict:fetch(ChildShort, Dict)),
                     Fun_labels = dict:to_list(Fun_dict),
                     {Key, [dict:from_list(lists:filter(fun({Label, [{G, State}]}) ->
                     Label /= RemoveLabel
                     end , Fun_labels))]};
             false -> {Key, Value}
           end
         end, dict:to_list(Dict))).

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


dict_delete(Short, Dict) -> 
  case dict:is_key(Short, Dict) of 
    true ->
      Keys = lists:filter(fun(Key) -> Key /= Short end,dict:fetch_keys(Dict)),
      dict:from_list(lists:map(fun(Key) ->
        Values = dict:fetch(Key, Dict),
        FilteredValues = lists:filter(fun(Value) -> Value /= Short end,Values),
        {Key, FilteredValues}
      end,Keys));
    false -> Dict
  end.
  
dict_search(Short, Dict) -> 
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
  if
    length(Filtered) == 0 -> Short;
    true -> lists:nth(1, Filtered)
  end.

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

send_request(Pid, Message) ->
  case is_process_alive(Pid) of
    true -> Pid ! Message;
    false -> throw("Process not alive")
  end.