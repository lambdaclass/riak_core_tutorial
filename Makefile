.PHONY: dev

dev:
	./rebar3 release && _build/default/rel/rc_example/bin/rc_example
