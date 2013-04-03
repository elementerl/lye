-module(lye).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([check/2]).

-type error() :: {error, Reason::binary()}.
-type structure() :: any().

-type spec_fun() :: fun((structure()) -> (ok | error())).
-type spec_mod() :: module().
-type spec()     :: spec_fun()| spec_mod().

-type fun_tup() :: {Fun::atom(), Arity::non_neg_integer()}.
-type module_extractor() :: fun((module()) -> [fun_tup()]).

%% =====================================================================
%%  API
%% =====================================================================

-spec check(structure(), Specs::[spec()]) -> ok | [error()].
check(Structure, Specs) ->
    SpecFuns = lists:map(fun(Spec) -> normalize_spec(Spec, fun module_exports/1) end, Specs),
    apply_specs(Structure, SpecFuns).

%% =====================================================================
%%  Private Interface
%% =====================================================================

-spec apply_specs(structure(), [spec_fun()]) -> ok | [error()].
apply_specs(Structure, SpecFuns) ->
    case lists:filter(fun is_error/1, lists:map(fun(F) -> F(Structure) end, SpecFuns)) of
        []  -> ok;
        Err -> Err
    end.

-spec normalize_spec(spec(), module_extractor()) -> [spec_fun()].
normalize_spec(Spec, _) when is_function(Spec, 1) ->
    [Spec];
normalize_spec(SpecMod, Extractor) when is_atom(SpecMod), is_function(Extractor, 1) ->
    lists:map(fun({Name, _}) ->
                      fun(Structure) -> apply(SpecMod, Name, [Structure]) end
              end, exported_specs(Extractor(SpecMod))).

-spec is_error(ok | error()) -> boolean().
is_error({error, _}) -> true;
is_error(ok) -> false.

-spec exported_specs([fun_tup()]) -> [fun_tup()].
exported_specs(ExportedFuns) ->
    {ok, RE} = re:compile("_spec$"),
    lists:filter(fun({F, _}) -> does_match(F, RE) end, ExportedFuns).

-spec does_match(Fun::atom(), RE::re:mp()) -> boolean().
does_match(Fun, RE) when is_atom(Fun) ->
    case re:run(atom_to_list(Fun), RE) of
        {match, _} -> true;
        nomatch    -> false
    end.

-spec module_exports(module()) -> [fun_tup()].
module_exports(M) when is_atom(M) ->
    M:module_info(exports).
