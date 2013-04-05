REBAR=`which rebar || which ./rebar`
DIALYZER=`which dialyzer`
ERL=`which erl`

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

app.plt:
	@$(DIALYZER) --build_plt --output_plt app.plt --apps erts kernel stdlib

dialyze: app.plt compile
	@$(DIALYZER) -q --plt app.plt -n ebin -Wunmatched_returns -Werror_handling \
		-Wrace_conditions -Wunderspecs

test:
	@$(REBAR) skip_deps=true eunit

validate: dialyze test

clean:
	@$(REBAR) clean

repl:
	@$(ERL) -pa ebin

.PHONY: all test clean validate dialyze deps
