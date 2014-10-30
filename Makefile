.PHONY: deps

all: deps compile

compile: deps
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean devclean relclean
	./rebar delete-deps

rel: all
	./rebar generate

relclean:
	rm -rf rel/riak_pg

stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/riak_pg/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/riak_pg/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/riak_pg/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/riak_pg/lib;)

DIALYZER_APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool eunit syntax_tools compiler mnesia public_key snmp

include tools.mk

typer:
	typer --annotate -I ../ --plt $(PLT) -r src
