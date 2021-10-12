-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).


%%% A non-symbolic generator for bst, parameterised by key and value generators
bst(Key, Value) ->
    ?LET(KVS, eqc_gen:list({Key, Value}),
         lists:foldl(fun({K,V}, T) -> insert(K, V, T) end,
                     empty(),
                     KVS)).

% example key and value generators
int_key() -> eqc_gen:int().
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).

int_value() -> eqc_gen:int().


%%% invariant properties

% all generated bst are valid
prop_arbitrary_valid() ->
    ?FORALL(T, bst(atom_key(), int_value()),
            valid(T)).

% if we insert into a valid tree it stays valid
prop_insert_valid() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            valid (insert(K, V, T))).

% check that an empty BST is valid
prop_empty_valid() ->
    valid (empty()).

% if we delete a key from a tree it's still valid
prop_delete_valid() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
        valid ( delete(K, insert(K, V, T)))).

% if we delete a key from a tree it's still valid
prop_union_valid() ->
    ?FORALL({T1, T2}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
        valid( union(T1, T2))).

%%% -- postcondition properties

prop_insert_post() ->
    ?FORALL({K1, K2, V, T},
            {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K2, insert(K1, V, T)),
                       case K1 =:= K2 of
                           true ->  {found, V};
                           false -> find(K2, T)
                       end)).

prop_find_post_present() ->
  % ∀ k v t. find k (insert k v t) === {found, v}
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K, insert(K, V, T)),
                       {found, V})).


prop_find_post_absent() ->
     % ∀ k t. find k (delete k t) === nothing
     ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K, delete(K, insert(K, V, T))),
                       nothing)).

prop_delete_post() ->
    ?FORALL({K1, K2, V, T},
        {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
        eqc:equals(find(K2, delete(K1, insert(K1, V, T))),
            case K1 =:= K2 of
                true ->  nothing;
                false -> find(K2, T)
            end)).

prop_union_post() ->
    ?FORALL({K, T1, T2},
        {atom_key(), bst(atom_key(), int_value()), bst(atom_key(), int_value())},
        eqc:equals(find(K, union(T1, T2)),
        case find(K, T1) of
            nothing -> 
                case find(K, T2) of
                    nothing -> nothing;
                    {found, V2} -> {found, V2}
                end;
            {found, V1} -> {found, V1}
        end)).
                       

%%% -- metamorphic properties

%% the size is larger after an insert
prop_size_insert() ->
    % ∀ k v t. size (insert k v t) >= size t
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            bst:size(insert(K, V, T)) >= bst:size(T)).

obs_equals(T1, T2) ->
     eqc:equals(to_sorted_list(T1), to_sorted_list(T2)).

prop_insert_insert() ->
    ?FORALL({K1, K2, V1, V2, T},
        {atom_key(), atom_key(), int_value(), int_value(),
        bst(atom_key(), int_value())},
        obs_equals(insert(K1, V1, insert(K2, V2, T)),
            case K1 =:= K2 of
                true ->  insert(K1, V1, T);
                false -> insert(K2, V2, insert(K1, V1, T))
            end)).

prop_delete_delete() ->
    ?FORALL({K1, K2, V1, V2, T},
        {atom_key(), atom_key(), int_value(), int_value(),
        bst(atom_key(), int_value())},
        obs_equals(delete( K1, delete(K2, insert(K1, V1, insert(K2, V2, T)))),
            case K1 =:= K2 of
                true ->  delete(K1, insert(K1, V1, insert(K2, V2, T)));
                false -> delete( K2, delete(K1, insert(K1, V1, insert(K2, V2, T))))
            end)).

prop_size_delete() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            bst:size(delete(K, insert(K, V, T))) =< bst:size(T)).

prop_size_delete2() ->
    ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            bst:size(delete(K, T)) =< bst:size(T)).


prop_union_union() ->
    ?FORALL({T1, T2, T3},
        {bst(atom_key(), int_value()), bst(atom_key(), int_value()), bst(atom_key(), int_value())},
        obs_equals(union(union(T1, T2), T3),
            if
                T1 == T2 -> union(T2, T3);
                T1 == T3 -> union(T1, T2);
                T2 == T3 -> union(T1, T3);
                true -> union(union(T1, T2), T3)
            end)).

%%% -- Model based properties
model(T) -> to_sorted_list(T).

prop_insert_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            equals(model(insert(K, V, T)),
                   sorted_insert(K, V, delete_key(K, model(T))))).

prop_find_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            equals(lists:keysearch(K, 1, model(insert(K, V, T))), 
                case find(K, insert(K, V, T)) of
                    nothing -> false;
                    {found, V1} -> {value, {K, V1}}
                end)).

prop_empty_model() ->
    equals(model(empty()), []).

prop_delete_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            equals(model(delete(K, insert(K, V, T))),
                   delete_key(K, sorted_insert(K, V, delete_key(K, model(T)))))).

prop_union_model() ->
    ?FORALL({T1, T2}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            equals(model(union(T1, T2)),
                lists:foldl(
                    fun({InKey, InVal}, T_Acc) ->
                        sorted_insert(InKey, InVal, delete_key(InKey, T_Acc)) end,
                        model(T2),
                        model(T1)
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
