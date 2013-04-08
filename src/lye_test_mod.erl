-module(lye_test_mod).

-export([foo/1, any_spec/1]).

foo(foo) -> ok;
foo(_) -> {error, not_foo}.

any_spec(_) -> ok.
