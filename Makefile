BASEDIR = $(shell pwd)
REBAR = rebar3

all: compile

compile:
	$(REBAR) compile

format:
	$(REBAR) format

clean:
	$(REBAR) clean

dialyzer:
	$(REBAR) dialyzer

test: 
	$(REBAR) eunit

rel:
	$(REBAR) release

run: rel
	_build/default/rel/rcl_bench/bin/rcl_bench foreground

results: rel
	R/summary.r
