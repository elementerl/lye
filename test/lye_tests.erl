-module(lye_tests).
-include_lib("eunit/include/eunit.hrl").

%% =====================================================================
%%  check/2
%% =====================================================================

check_test_() ->
    Structure = {tome_of_light_naps, [{price_dollars, 300}, {weight_kg, 23.87}]},

    PassingSpec = fun({tome_of_light_naps, Props}) ->
                          case proplists:get_value(price_dollars, Props) >= 100 of
                              true  -> ok;
                              false -> {error, selling_for_too_little}
                          end
                  end,
    FailingSpec = fun({tome_of_light_naps, Props}) ->
                          case proplists:get_value(weight_kg, Props) < 1 of
                              true  -> ok;
                              false -> {error, selling_for_too_much}
                          end
                  end,
    OtherFailingSpec = fun({tome_of_light_naps, Props}) ->
                               Price = proplists:get_value(price_dollars, Props),
                               Weight = proplists:get_value(weight_kg, Props),

                               case Weight / Price =< 0.01 of
                                   true -> ok;
                                   false -> {error, <<"I will pay no more than a penny a kg!">>}
                               end
                       end,

    [
     { "it will return ok when there are no specs given",
       ?_assertMatch(ok, lye:check(Structure, []))},

     { "it will correctly signal failure and success of specs",
       [
        ?_assertMatch(ok, lye:check(Structure, [PassingSpec])),
        ?_assertMatch({error, [selling_for_too_much]},
                      lye:check(Structure, [FailingSpec])),
        ?_assertMatch({error, [selling_for_too_much]},
                      lye:check(Structure, [FailingSpec, PassingSpec])),
        ?_assertMatch({error, [selling_for_too_much, <<"I will pay no more than a penny a kg!">>]},
                      lye:check(Structure, [FailingSpec, PassingSpec, OtherFailingSpec]))
       ]
     }
    ].

%% =====================================================================
%% Override spec pattern
%% =====================================================================

overriding_spec_pattern_test_() ->
    application:set_env(lye, spec_pattern, ".*"),

    { "looks for the spec_pattern env var before using the default",
        [
            ?_assertMatch(ok, lye:check(foo, [lye_test_mod])),
            ?_assertMatch({error, [not_foo]}, lye:check(bar, [lye_test_mod]))
        ]
    }.

%% =====================================================================
%%  apply_specs/2
%% =====================================================================

apply_specs_test_() ->
    Structure = {pancake_helmet, [{price_dollars, 1000}, {weight_kg, 2.3}]},

    FailingSpec = fun({pancake_helmet, Props}) ->
                          case proplists:get_value(price_dollars, Props) > 1 of
                              true  -> {error, price_is_too_damn_high};
                              false -> ok
                          end
                  end,
    PassingSpec = fun({pancake_helmet, Props}) ->
                          case proplists:get_value(weight_kg, Props) > 3 of
                              true  -> {error, oh_god_i_cannot_carry_that};
                              false -> ok
                          end
                  end,

    [
     { "returns ok when no specs are given",
       ?_assertMatch(ok, lye:apply_specs(Structure, []))},

     { "returns ok when all specs pass",
       ?_assertMatch(ok, lye:apply_specs(Structure, [PassingSpec]))},

     { "signals error when a spec fails",
       [
        ?_assertMatch({error, [price_is_too_damn_high]},
                      lye:apply_specs(Structure, [FailingSpec])),
        ?_assertMatch({error, [price_is_too_damn_high]},
                      lye:apply_specs(Structure, [FailingSpec, PassingSpec]))
       ]
     }
    ].

%% =====================================================================
%%  normalize_spec/2
%% =====================================================================

normalize_spec_test_() ->
    SpecFun = fun(_) -> ok end,
    BadSpecFun = fun(_, _) -> ok end,
    SpecMod = mod,
    Extractor = fun(mod) -> [{a_fun_spec, 1}, {another_fun_spec, 1}] end,

    [
     { "simply bundles a spec function up into a list",
       ?_assertMatch([SpecFun], lye:normalize_spec(SpecFun, Extractor))},

     { "returns a list of functions returns by the Extractor",
       ?_assertMatch([_|_], lye:normalize_spec(SpecMod, Extractor))},

     { "raises an error when a spec function does not have arity 1",
       ?_assertError(function_clause, lye:normalize_spec(BadSpecFun, Extractor))}
    ].

%% =====================================================================
%%  is_error/2
%% =====================================================================

is_error_test_() ->
    [
     { "identifies errors and otherwise",
       [
        ?_assertMatch(true, lye:is_error({error, <<"bad things!">>})),
        ?_assertMatch(false, lye:is_error(ok))
       ]
     }
    ].

%% =====================================================================
%%  exported_specs/2
%% =====================================================================

exported_specs_test_() ->
    Arity = 1,
    SpecPat = "_spec$",

    NoSpecs        = [{a_fun, Arity}],
    BadlyNamedSpec = [{spec_fun, Arity}],
    SomeSpecs      = [{a_fun_spec, Arity} | NoSpecs],
    AllSpecs       = [{a_spec, Arity}, {b_spec, Arity}],

    [
     { "It should return empty list when no specs are defined",
       [
        ?_assertMatch([], lye:exported_specs(NoSpecs, SpecPat)),
        ?_assertMatch([], lye:exported_specs(BadlyNamedSpec, SpecPat))
       ]
     },

     { "It should be identity on module that exports only specs",
       ?_assertMatch(AllSpecs, lye:exported_specs(AllSpecs, SpecPat))},

     { "It should only return specs from a mixed bag",
       ?_assertMatch([{a_fun_spec, Arity}], lye:exported_specs(SomeSpecs, SpecPat))}
    ].


%% =====================================================================
%%  does_match/2
%% =====================================================================

does_match_test_() ->
    {ok, RE} = re:compile("banana"),

    [
     { "It returns true for a matching atom",
       ?_assertMatch(true, lye:does_match(banana, RE))},

     { "It returns false for an atom that does not match",
       ?_assertMatch(false, lye:does_match(does_not_match, RE))}
    ].
