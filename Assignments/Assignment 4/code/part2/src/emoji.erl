-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

-spec start([{string(), binary()}]) -> any().
start(Initial) ->
  case length(Initial) == sets:size(sets:from_list(Initial)) of
    true -> spawn(fun() ->
      loop({dict:from_list(Initial), dict:new()})  
    end);
    false -> {error, "Initial elements contains duplicates"}
  end.

-spec new_shortcode(string(), string(), binary()) -> any().
new_shortcode(E, Short, Emo) -> 
  Me = self(),
  E ! {register, Me, Short, Emo},
  receive
    RetVal -> RetVal
  end.

-spec alias(string(), string(), string()) -> any().
alias(E, Short1, Short2) -> 
  Me = self(),
  E ! {alias, Me, Short1, Short2},
  receive
    RetVal -> RetVal
  end.

-spec delete(string(), string()) -> any().
delete(E, Short) -> E ! {delete, Short}.

-spec lookup(string(), string()) -> any().
lookup(E, Short) ->
  Me = self(),
  E ! {lookup, Me, Short},
  receive
    RetVal -> RetVal
  end.

analytics(_, _, _, _, _) -> not_implemented.

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

stop(_) -> not_implemented.

loop({Shortcodes, Alias} = State) ->
receive
  {register, From, Short, Emo} ->
    case dict:is_key(Short, Shortcodes) of
      true -> From ! {error, "Short already exists"};
      false -> From ! ok, loop({dict:append(Short, Emo, Shortcodes), Alias})
    end;
  {alias, From, Short1, Short2} ->
    case dict:is_key(Short1, Shortcodes) of
      true -> From ! ok, loop({Shortcodes, dict:append(Short1, Short2, Alias)});
      false -> case dict_search_for_val(Short1, Alias) of
        true -> From ! ok, loop({Shortcodes, dict:append(Short1, Short2, Alias)});
        false -> From ! {error, "Short1 not registered"}
      end
    end;
  {delete, Short} ->
    loop({dict:erase(Short, Shortcodes), dict_delete(Short, Alias)});
  {lookup, From, Short} ->
    case dict:is_key(Short, Shortcodes) of
      true -> 
        Emo = dict:fetch(Short, Shortcodes),
        From ! {ok, Emo},
        loop(State);
      false -> case dict_search(Short, Alias) of
        Short1 ->
          case dict:is_key(Short1, Shortcodes) of
            true -> 
              Emo = dict:fetch(Short1, Shortcodes),
              From ! {ok, Emo};
            false -> From ! no_emoji
          end
      end
    end,
    loop(State)
end.

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



% delete(Short 2)

%Shortcode:
% Short 1 -> Emoji

% Alias:
%Short 1 -> [Short 2]
%Short 2 -> [Short 3]
%Short 2' -> [Short 3]
%Short 3 -> [Short 4]
%Short 4 -> [Short 5]