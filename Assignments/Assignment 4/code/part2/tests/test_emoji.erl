-module(test_emoji).

-export([test_all/0]).

% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"Functionality tests", spawn,
       [ test_start_server()
       , test_start_server_duplicates()
       , test_shortcode_smiley()
       , test_lookup_smiley()
       , test_lookup_nonExisting()
       , test_lookup_more()
       , test_alias_lookup()
       , test_more_aliases()
       , test_delete_shortcode()
       , test_delete_shortcode_and_alias()
       , test_delete_many_alias()
       , test_add_analytics()
       , test_add_two_analytics()
       , test_add_same_analytics()
       , test_run_analytics()
       , test_lookup_run_analytics()
       , test_alias_run_analytics()
       , test_alias_run_analytics_two_fun()
       , test_remove_analytics()
       , test_remove_analytics_alias()
       , test_remove_first_analytics_alias()
       , test_remove_first_analytics_alias1()
       , test_stop_server()
       ]
      },
      {"Stress test with many emojis", spawn,
      [ test_start_server_medium()
      , test_shortcode_mathias_medium()
      , test_lookup_more_medium()
      , test_add_funs_medium()
      , test_run_funs_medium()
      , test_remove_all_funs()
      , test_add_aliases_medium()
      , test_remove_aliases_medium()
      , test_stop_huge_server()]}
    ].

%%%%%%%%%% FUNCTIONS USED IN TESTS %%%%%%%%%%

counter(_,N) -> N + 1.

nothing(_,N) -> N.

err(_,_) -> throw("err").


%%%%%%%%%% TESTS WITH FEW EMOJIS %%%%%%%%%%

test_start_server() ->
    {"We can call start/1 and it does not crash",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([]))
     end }.

test_start_server_duplicates() ->
  {"We can call start/1 and it does crash :-)",
  fun () ->
    ?assertMatch({error, _}, emoji:start([{"same", <<123>>},{"same", <<123>>}]))
  end }.

test_shortcode_smiley() ->
    {"Register new shortcode",
     fun () ->
       {ok, S} = emoji:start([]),
       ?assertEqual(ok, emoji:new_shortcode(S, "smiley",
                                            <<240,159,152,131>>))
     end }.

test_lookup_smiley() ->
    {"Register new shortcode and look it up",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "smiley"))
    end }.

test_lookup_nonExisting() ->
    {"Lookup non-existing shortcode",
    fun () ->
      {ok, S} = emoji:start([]),
      ?assertEqual(no_emoji, emoji:lookup(S, "AP"))
    end }.

test_lookup_more() ->
    {"Register three shortcodes and look them up",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:new_shortcode(S, "accept", <<240,159,137,145>>),
      {ok} = emoji:new_shortcode(S, "100", <<240,159,146,175>>),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "smiley")),
      ?assertEqual({ok,<<240,159,137,145>>}, emoji:lookup(S, "accept")),
      ?assertEqual({ok,<<240,159,146,175>>}, emoji:lookup(S, "100"))
    end }.


test_alias_lookup() ->
    {"Add an alias and look it up",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias(S, "smiley", "yelims"),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "yelims"))
    end }.

test_more_aliases() ->
    {"Add two aliases to smiley and look them up",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias(S, "smiley", "yelims"),
      {ok} = emoji:alias(S, "smiley", "leysmi"),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "yelims")),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "leysmi"))
    end }.

test_delete_shortcode() ->
    {"Register new shortcode and delete it",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {_} = emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end }.  

test_delete_shortcode_and_alias() ->
    {"Register new shortcode, add alias and delete them",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias(S, "smiley", "yelims"),
      {_} = emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
      ?assertEqual(no_emoji, emoji:lookup(S, "yelims"))
    end }.

test_delete_many_alias() ->
    {"Register new shortcode, add alias, add alias to alias and delete 1st alias",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias(S, "smiley", "yelims"),
      {ok} = emoji:alias(S, "yelims", "miley"),
      {_} = emoji:delete(S, "yelims"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
      ?assertEqual(no_emoji, emoji:lookup(S, "yelims")),
      ?assertEqual(no_emoji, emoji:lookup(S, "miley"))
    end }.

test_add_analytics() ->
    {"Add an analytics function to smiley",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      ?assertEqual(ok, emoji:analytics(S, "smiley",fun counter/2, "counter", 0))
    end }.

test_add_two_analytics() ->
    {"Add two analytics function to smiley",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      ?assertEqual(ok, emoji:analytics(S, "smiley",fun counter/2, "counter", 0)),
      ?assertEqual(ok, emoji:analytics(S, "smiley",fun nothing/2, "nothing", 0))
    end }.

test_add_same_analytics() ->
    {"Add two identical analytics function to smiley",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:analytics(S, "smiley",fun counter/2, "counter", 0),
      ?assertMatch({error, _}, emoji:analytics(S, "smiley",fun counter/2, "counter", 0))
    end }.

test_run_analytics() ->
    {"Add an analytics function to smiley and get the stats",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:analytics(S, "smiley", fun counter/2, "counter", 0),
      ?assertEqual({ok,[{"counter", 0}]}, emoji:get_analytics(S, "smiley"))
    end }.

test_lookup_run_analytics() ->
    {"Add an analytics function to smiley, lookup and get the stats",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:analytics(S, "smiley", fun counter/2, "counter", 0),
      ?assertEqual({ok,[{"counter", 0}]}, emoji:get_analytics(S, "smiley")),
      {ok, _} = emoji:lookup(S, "smiley"),
      ?assertEqual({ok,[{"counter", 1}]}, emoji:get_analytics(S, "smiley"))
    end }.

test_alias_run_analytics() ->
    {"Add an analytics function to smiley, add alias, lookup alias and get the stats",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias("smiley", "yelims"),
      {ok} = emoji:analytics(S, "smiley", fun counter/2, "counter", 0),
      ?assertEqual({ok,[{"counter", 0}]}, emoji:get_analytics(S, "smiley")),
      {ok, _} = emoji:lookup(S, "yelims"),
      ?assertEqual({ok,[{"counter", 1}]}, emoji:get_analytics(S, "smiley"))
    end }.

test_alias_run_analytics_two_fun() ->
    {"Add two analytics function to smiley, add alias, lookup alias and get the stats",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias("smiley", "yelims"),
      {ok} = emoji:analytics(S, "smiley", fun counter/2, "counter", 0),
      {ok} = emoji:analytics(S, "smiley", fun nothing/2, "nothing", "erlang"),
      ?assertEqual({ok,[{"counter", 0}, {"nothing", "erlang"}]},
                                            emoji:get_analytics(S, "smiley")),
      {ok, _} = emoji:lookup(S, "yelims"),
      ?assertEqual({ok,[{"counter", 1}, {"nothing", "erlang"}]},
                                           emoji:get_analytics(S, "smiley"))
    end }.

test_remove_analytics() ->
    {"Add an analytics function to smiley and delete it",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:analytics(S, "smiley", fun counter/2, "counter", 0),
      {_} = emoji:remove_analytics(S, "smiley", "counter"),
      ?assertMatch({error, _}, emoji:get_analytics(S, "smiley"))
    end }.

test_remove_analytics_alias() ->
    {"Add an analytics function to smiley, add alias and delete it for alias",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias("smiley", "yelims"),
      {ok} = emoji:analytics(S, "smiley", fun counter/2, "counter", 0),
      {_} = emoji:remove_analytics(S, "yelims", "counter"),
      ?assertMatch({error, _}, emoji:get_analytics(S, "smiley"))
    end }.

test_remove_first_analytics_alias() ->
    {"Add two analytics functions to smiley, add alias and delete 1st for alias",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias("smiley", "yelims"),
      {ok} = emoji:analytics(S, "smiley", fun counter/2, "counter", 0),
      {ok} = emoji:analytics(S, "smiley", fun nothing/2, "nothing", "erlang"),
      {_} = emoji:remove_analytics(S, "yelims", "counter"),
      ?assertMatch([{"nothing", "erlang"}], emoji:get_analytics(S, "smiley"))
    end }.

test_remove_first_analytics_alias1() ->
    {"Add two analytics functions to smiley, add alias and delete 1st for alias",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias("smiley", "yelims"),
      {ok} = emoji:analytics(S, "smiley", fun counter/2, "counter", 0),
      {ok} = emoji:analytics(S, "smiley", fun nothing/2, "nothing", "erlang"),
      {_} = emoji:remove_analytics(S, "yelims", "counter"),
      ?assertMatch([{"nothing", "erlang"}], emoji:get_analytics(S, "yelims"))
    end }.

test_stop_server() ->
    {"End server",
    fun () ->
      {ok,S} = emoji:start([]),
      ?assertMatch(ok, emoji:stop(S))
    end }.  

%%TODO: ADD TEST WITH UNRELIABLE FUNCTION: err/2.

%%%%%%%%%% TESTS WITH MANY EMOJIS %%%%%%%%%%
test_start_server_medium() ->
    {"We can call start/1 and it does not crash with many emojis",
     fun () ->
       Emos = someemoji:medium(),
       ?assertMatch({ok, _}, emoji:start(Emos))
     end }.

test_shortcode_mathias_medium() ->
    {"Register new shortcode",
     fun () ->
       Emos = someemoji:medium(),
       {ok, S} = emoji:start(Emos),
       ?assertEqual(ok, emoji:new_shortcode(S, "mathias",
                                            <<240,159,152,131>>))
     end }.

test_lookup_more_medium() ->
    {"Lookup three shortcodes from 'medium' list",
    fun () ->
      Emos = someemoji:medium(),
      {ok, S} = emoji:start(Emos),
      ?assertEqual({ok,<<240,159,142,155>>}, emoji:lookup(S, "control knobs")),
      ?assertEqual({ok,<<240,159,164,167>>}, emoji:lookup(S, "sneezing face")),
      ?assertEqual({ok,<<240,159,142,145>>}, emoji:lookup(S, "moon viewing ceremony"))
    end }.

test_add_funs_medium() ->
    {"Add analytics fun to every emoji",
    fun () ->
      Emos = someemoji:medium(),
      {ok, S} = emoji:start(Emos),
      lists:foreach(fun({Short, _}) ->
        ?assertEqual(ok, emoji:analytics(S, Short, fun counter/2, "counter", 0))
         end, Emos)
    end }.

test_run_funs_medium() ->
    {"Add analytics fun to every emoji",
    fun () ->
      Emos = someemoji:medium(),
      {ok, S} = emoji:start(Emos),
      lists:foreach(fun({Short, _}) ->
         emoji:analytics(S, Short, fun counter/2, "counter", 0)
         end, Emos),
      lists:foreach(fun({Short, _}) ->
        ?assertEqual([{"counter", 0}], emoji:get_analytics(S, Short))
         end, Emos)
    end }.

test_remove_all_funs() ->
    {"Add analytics fun to every emoji and remove it",
    fun () ->
      Emos = someemoji:medium(),
      {ok, S} = emoji:start(Emos),
      lists:foreach(fun({Short, _}) ->
         emoji:analytics(S, Short, fun counter/2, "counter", 0)
         end, Emos),
      lists:foreach(fun({Short, _}) ->
        ?assertMatch(ok, emoji:remove_analytics(S, Short))
         end, Emos)
    end }.

test_add_aliases_medium() ->     
    {"Add two aliases to 'sneezing face' and look them up",
    fun () ->
      Emos = someemoji:medium(),
      {ok, S} = emoji:start(Emos),
      {ok} = emoji:alias(S, "sneezing face", "me during AP"),
      {ok} = emoji:alias(S, "sneezing face", "not me after AP"),
      ?assertEqual({ok,<<240,159,164,167>>}, emoji:lookup(S, "me during AP")),
      ?assertEqual({ok,<<240,159,164,167>>}, emoji:lookup(S, "not me after AP"))
    end }.

test_remove_aliases_medium() ->     
    {"Add two aliases to 'sneezing face' and and removing them all",
    fun () ->
      Emos = someemoji:medium(),
      {ok, S} = emoji:start(Emos),
      {ok} = emoji:alias(S, "sneezing face", "me during AP"),
      {ok} = emoji:alias(S, "me during AP", "not me after AP"),
      {_} = emoji:delete(S, "me during AP"),
      ?assertEqual(no_emoji, emoji:lookup(S, "sneezing face")),
      ?assertEqual(no_emoji, emoji:lookup(S, "me during AP")),
      ?assertEqual(no_emoji, emoji:lookup(S, "not me after AP"))
    end }.

test_stop_huge_server() ->
    {"Stop server full of emojis",
    fun () ->
      Emos = someemoji:medium(),
      {ok, S} = emoji:start(Emos),
      ?assertMatch(ok, emoji:stop(S))
    end }.
