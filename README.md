# lye

[![Build Status](https://travis-ci.org/elementerl/lye.png)](https://travis-ci.org/elementerl/lye)

A validation library. lye is [semantically versioned](http://semver.org/).

## Usage

`lye:check/2` takes some structure as its first argument, a list of singleton
functions or a module that exports a bunch singleton functions ending in
`_spec` and maps these functions over the structure.

```
> make repl
Erlang R15B02 (erts-5.9.2) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.2  (abort with ^G)
1> ValidStructure = {network_host, [{latency_us, 5}, {dns_name, <<"foo.example.com">>}]}.
{network_host,[{latency_us,5},
               {dns_name,<<"foo.example.com">>}]}
2> NoLatencyStructure = {network_host, [{dns_name, <<"nolat.example.com">>}]}.
{network_host,[{dns_name,<<"nolat.example.com">>}]}
3> NoNameStructure = {network_host, [{latency_us, 10}]}.
{network_host,[{latency_us,10}]}
4> MustHaveDnsNameSpec = fun({network_host, Props}) -> case proplists:is_defined(dns_name, Props) of true -> ok; false -> {error, <<"must have dns_name">>} end end.
#Fun<erl_eval.6.82930912>
5> MustHaveLatencySpec = fun({network_host, Props}) -> case proplists:is_defined(latency_us, Props) of true -> ok; false -> {error, <<"must have latency">>} end end.
#Fun<erl_eval.6.82930912>
6> LatencyMustBeLessThan9usSpec = fun({network_host, Props}) -> case proplists:get_value(latency_us, Props, infinity) < 9 of true -> ok; false -> {error, latency_too_high} end end.
#Fun<erl_eval.6.82930912>
7> lye:check(ValidStructure, [MustHaveDnsNameSpec, MustHaveLatencySpec, LatencyMustBeLessThan9usSpec]).
ok
8> lye:check(NoLatencyStructure, [MustHaveDnsNameSpec, MustHaveLatencySpec, LatencyMustBeLessThan9usSpec]).
[{error,<<"must have latency">>},{error,latency_too_high}]
9> lye:check(NoNameStructure, [MustHaveDnsNameSpec, MustHaveLatencySpec, LatencyMustBeLessThan9usSpec]).
[{error,<<"must have dns_name">>},{error,latency_too_high}]
10>
```

`lay:check/2` is much nicer to work with when you aren't working entirely in the
REPL.

## License

lye is released under the [MIT license](http://opensource.org/licenses/MIT). See
LICENSE.md for more details.
