-module(host_collection).

-export([must_have_dns_name_spec/1, must_have_latency_spec/1]).
-export([latency_must_be_less_than_9us_spec/1]).

must_have_dns_name_spec({network_host, Props}) ->
    case proplists:is_defined(dns_name, Props) of
        true -> ok;
        false -> {error, <<"must have dns_name">>}
    end.

must_have_latency_spec({network_host, Props}) ->
    case proplists:is_defined(latency_us, Props) of
        true -> ok;
        false -> {error, <<"must have latency">>}
    end.

latency_must_be_less_than_9us_spec({network_host, Props}) ->
    case proplists:get_value(latency_us, Props, infinity) < 9 of
        true -> ok;
        false -> {error, latency_too_high}
    end.
