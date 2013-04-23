%% This module is a test fixture. We put it here because generating it on the
%% fly, or using CT just because we need a module to play around with, is more
%% trouble than it is worth.
-module(lye_test_mod).

-export([foo/1, any_spec/1]).

foo(foo) -> ok;
foo(_) -> {error, not_foo}.

any_spec(_) -> ok.
