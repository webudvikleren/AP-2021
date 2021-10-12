-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).


%%% A symbolic generator for bst.
bst() ->
    ?LAZY(
     frequency([{1,{call,bst,empty, []}},
                {4,?LETSHRINK([T],[bst()],
                        {call,bst,insert,[atom_key(),int_value(),T]})}])).

% example key and value generators
int_key() -> eqc_gen:int().
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).

int_value() -> eqc_gen:int().


%%% invariant properties

% all generated bst are valid
prop_arbitrary_valid() ->
    ?FORALL(T, bst(),
            valid(eval(T))).

% if we insert into a valid tree it stays valid
prop_insert_valid() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
            valid (insert(K, V, eval(T)))).

% check that an empty BST is valid
prop_empty_valid() ->
    valid (empty()).

% if we delete a key from a tree it's still valid
prop_delete_valid() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
        valid ( delete(K, insert(K, V, eval(T))))).

% if we delete a key from a tree it's still valid
prop_union_valid() ->
    ?FORALL({T1, T2}, {bst(), bst()},
        valid( union(eval(T1), eval(T2)))).

%%% -- postcondition properties

prop_insert_post() ->
    ?FORALL({K1, K2, V, T},
            {atom_key(), atom_key(), int_value(), bst()},
            eqc:equals(find(K2, insert(K1, V, eval(T))),
                       case K1 =:= K2 of
                           true ->  {found, V};
                           false -> find(K2, eval(T))
                       end)).

prop_find_post_present() ->
  % ∀ k v t. find k (insert k v t) === {found, v}
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
            eqc:equals(find(K, insert(K, V, eval(T))),
                       {found, V})).


prop_find_post_absent() ->
     % ∀ k t. find k (delete k t) === nothing
     ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
            eqc:equals(find(K, delete(K, insert(K, V, eval(T)))),
                       nothing)).

prop_delete_post() ->
    ?FORALL({K1, K2, V, T},
        {atom_key(), atom_key(), int_value(), bst()},
        eqc:equals(find(K2, delete(K1, insert(K1, V, eval(T)))),
            case K1 =:= K2 of
                true ->  nothing;
                false -> find(K2, eval(T))
            end)).

prop_union_post() ->
    ?FORALL({K, T1, T2},
        {atom_key(), bst(), bst()},
        eqc:equals(find(K, union(eval(T1), eval(T2))),
        case find(K, eval(T1)) of
            nothing -> 
                case find(K, eval(T2)) of
                    nothing -> nothing;
                    {found, V2} -> {found, V2}
                end;
            {found, V1} -> {found, V1}
        end)).
                       

%%% -- metamorphic properties

%% the size is larger after an insert
prop_size_insert() ->
    % ∀ k v t. size (insert k v t) >= size t
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
            bst:size(insert(K, V, eval(T))) >= bst:size(eval(T))).

obs_equals(T1, T2) ->
     eqc:equals(to_sorted_list(eval(T1)), to_sorted_list(eval(T2))).

prop_insert_insert() ->
    ?FORALL({K1, K2, V1, V2, T},
        {atom_key(), atom_key(), int_value(), int_value(),
        bst()},
        obs_equals(insert(K1, V1, insert(K2, V2, eval(T))),
            case K1 =:= K2 of
                true ->  insert(K1, V1, eval(T));
                false -> insert(K2, V2, insert(K1, V1, eval(T)))
            end)).

prop_delete_delete() ->
    ?FORALL({K1, K2, V1, V2, T},
        {atom_key(), atom_key(), int_value(), int_value(),
        bst()},
        obs_equals(delete( K1, delete(K2, insert(K1, V1, insert(K2, V2, eval(T))))),
            case K1 =:= K2 of
                true ->  delete(K1, insert(K1, V1, insert(K2, V2, eval(T))));
                false -> delete( K2, delete(K1, insert(K1, V1, insert(K2, V2, eval(T)))))
            end)).

prop_size_delete() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
            bst:size(delete(K, insert(K, V, eval(T)))) =< bst:size(eval(T))).

prop_size_delete2() ->
    ?FORALL({K, T}, {atom_key(), bst()},
            bst:size(delete(K, eval(T))) =< bst:size(eval(T))).


prop_union_union() ->
    ?FORALL({T1, T2, T3},
        {bst(), bst(), bst()},
        obs_equals(union(union(eval(T1), eval(T2)), eval(T3)),
            if
                T1 == T2 -> union(eval(T2), eval(T3));
                T1 == T3 -> union(eval(T1), eval(T2));
                T2 == T3 -> union(eval(T1), eval(T3));
                true -> union(union(eval(T1), eval(T2)), eval(T3))
            end)).

%%% -- Model based properties
model(T) -> to_sorted_list(T).

prop_insert_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
            equals(model(insert(K, V, eval(T))),
                   sorted_insert(K, V, delete_key(K, model(eval(T)))))).

prop_find_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
            equals(lists:keysearch(K, 1, model(insert(K, V, eval(T)))), 
                case find(K, insert(K, V, eval(T))) of
                    nothing -> false;
                    {found, V1} -> {value, {K, V1}}
                end)).

prop_empty_model() ->
    equals(model(empty()), []).

prop_delete_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
            equals(model(delete(K, insert(K, V, eval(T)))),
                   delete_key(K, sorted_insert(K, V, delete_key(K, model(eval(T))))))).

prop_union_model() ->
    ?FORALL({T1, T2}, {bst(), bst()},
            equals(model(union(eval(T1), eval(T2))),
                lists:foldl(
                    fun({InKey, InVal}, T_Acc) ->
                        sorted_insert(InKey, InVal, delete_key(InKey, T_Acc)) end,
                        model(eval(T2)),
                        model(eval(T1))
                )
            )
    ).

-spec delete_key(Key, [{Key, Value}]) -> [{Key, Value}].
delete_key(Key, KVS) -> [ {K, V} || {K, V} <- KVS, K =/= Key ].

-spec sorted_insert(Key, Value, [{Key, Value}]) -> nonempty_list({Key, Value}).
sorted_insert(Key, Value, [{K, V} | Rest]) when K < Key ->
    [{K, V} | sorted_insert(Key, Value, Rest)];
sorted_insert(Key, Value, KVS) -> [{Key, Value} | KVS].



%% -- Test all properties in the module: eqc:module(test_bst)
