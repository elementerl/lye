-module(lye).

-export([check/2, check/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-type reason() :: binary().
-type error() :: {error, reason()}.
-type errors() :: {error, list(reason())}.
-type structure() :: any().

-type spec_fun() :: fun((structure()) -> (ok | error())).
-type spec_mod() :: module().
-type spec()     :: spec_fun()| spec_mod().

-type fun_tup() :: {Fun::atom(), Arity::non_neg_integer()}.
-type module_extractor() :: fun((module()) -> [fun_tup()]).
-type spec_pattern() :: iodata().

-define(SPEC_PATTERN, "_spec$").

%% =====================================================================
%%  API
%% =====================================================================

-spec check(structure(), [spec()]) -> ok | errors().
check(Structure, Specs) when is_list(Specs) ->
    check(Structure, Specs, lookup_spec_pattern()).

-spec check(structure(), Specs::[spec()], SP::spec_pattern()) -> ok | errors().
check(Structure, Specs, SP) when is_list(Specs) ->
    Extractor = fun(Mod) -> module_exports(Mod, SP) end,
    SpecFuns = lists:flatten(lists:map(fun(Spec) -> normalize_spec(Spec, Extractor) end, Specs)),
    apply_specs(Structure, SpecFuns).

%% =====================================================================
%%  Private Interface
%% =====================================================================

-spec apply_specs(structure(), [spec_fun()]) -> ok | errors().
apply_specs(Structure, SpecFuns) ->
    case lists:filter(fun is_error/1, lists:map(fun(F) -> F(Structure) end, SpecFuns)) of
        []  -> ok;
        Errs -> {error, [E || {error, E} <- Errs]}
    end.

-spec normalize_spec(spec(), module_extractor()) -> nonempty_list(spec_fun()).
normalize_spec(Spec, _) when is_function(Spec, 1) ->
    [Spec];
normalize_spec(SpecMod, Extractor) when is_atom(SpecMod), is_function(Extractor, 1) ->
    lists:map(fun({Name, _}) ->
                      fun(Structure) -> apply(SpecMod, Name, [Structure]) end
              end, Extractor(SpecMod)).

-spec is_error(ok | error()) -> boolean().
is_error({error, _}) -> true;
is_error(ok) -> false.

-spec module_exports(M::atom(), SP::spec_pattern()) -> [fun_tup()].
module_exports(M, SP) when is_atom(M) ->
    exported_specs(M:module_info(exports), SP).

-spec exported_specs([fun_tup()], SP::spec_pattern()) -> [fun_tup()].
exported_specs(ExportedFuns, SP) ->
    {ok, RE} = re:compile(SP),
    lists:filter(fun({F, _}) -> does_match(F, RE) and not builtin(F) end, ExportedFuns).

-spec does_match(Fun::atom(), RE::re:mp()) -> boolean().
does_match(Fun, RE) when is_atom(Fun) ->
    case re:run(atom_to_list(Fun), RE) of
        {match, _} -> true;
        nomatch    -> false
    end.

-spec builtin(atom()) -> boolean().
builtin(module_info) -> true;
builtin(_) -> false.

-spec lookup_spec_pattern() -> spec_pattern().
lookup_spec_pattern() ->
    case application:get_env(lye, spec_pattern) of
        undefined     -> ?SPEC_PATTERN;
        {ok, Pattern} -> Pattern
    end.
