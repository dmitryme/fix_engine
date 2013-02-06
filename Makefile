.PHONY: deps test

all: deps compile

compile:
	./rebar compile
	mkdir -p ebin ebin/log
	cp apps/example/ebin/* ebin
	cp deps/fix_parser/fix_descr/fix.4.4.xml ebin

deps:
	./rebar get-deps

clean:
	./rebar clean
	rm ebin/*.beam
	rm -rf ebin/log

distclean: clean
	./rebar delete-deps

test: all
	./rebar skip_deps=true eunit

docs:
	./rebar skip_deps=true doc

dialyzer: compile
	@dialyzer -Wno_return -c apps/fix_engine/ebin
