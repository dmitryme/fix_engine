.PHONY: deps test

all: compile

compile:
	./rebar compile

example: example-deps
	./rebar -C rebar.example.config compile

example-deps: deps
	./rebar -C rebar.example.config get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

test: all
	ERL_LIBS=apps ./rebar skip_deps=true eunit ct

docs:
	./rebar skip_deps=true doc

dialyzer: compile
	@dialyzer -Wno_return -c apps/fix_engine/ebin
