.PHONY: deps test

fix_parser_deps:
	if [ ! -d apps/fix_parser/.fix_parser ]; then \
		git clone git://github.com/dmitryme/fix_parser.git apps/fix_parser/.fix_parser; \
		cp -r apps/fix_parser/.fix_parser/bind/erlang/fix_parser/src apps/fix_parser; \
		cp -r apps/fix_parser/.fix_parser/bind/erlang/fix_parser/include apps/fix_parser; \
		cp -r apps/fix_parser/.fix_parser/bind/erlang/fix_parser/c_src apps/fix_parser; \
		cp -r apps/fix_parser/.fix_parser/fix_descr apps/fix_parser; \
		cd apps/fix_parser/.fix_parser/build/; \
		./build.sh; \
	fi

fix_parser_deps_update:
	if [ ! -d apps/fix_parser/.fix_parser ]; then \
		git clone git://github.com/dmitryme/fix_parser.git apps/fix_parser/.fix_parser; \
		cp -r apps/fix_parser/.fix_parser/bind/erlang/fix_parser/src apps/fix_parser; \
		cp -r apps/fix_parser/.fix_parser/bind/erlang/fix_parser/include apps/fix_parser; \
		cp -r apps/fix_parser/.fix_parser/bind/erlang/fix_parser/c_src apps/fix_parser; \
		cp -r apps/fix_parser/.fix_parser/fix_descr apps/fix_parser; \
	else \
		cd apps/fix_parser/.fix_parser/; \
		git pull; \
		cd build; \
		./build.sh; \
	fi

all: deps compile

compile:
	./rebar compile

deps: fix_parser_deps
	./rebar get-deps

update-deps: fix_parser_deps_update
	./rebar update-deps

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
