.PHONY: dev

dev1:
	./rebar3 as dev1 release && _build/dev1/rel/rc_example/bin/rc_example

dev2:
	./rebar3 as dev2 release && _build/dev2/rel/rc_example/bin/rc_example

dev3:
	./rebar3 as dev3 release && _build/dev3/rel/rc_example/bin/rc_example

dev4:
	./rebar3 as dev4 release && _build/dev4/rel/rc_example/bin/rc_example

clean_data:
	rm -rf _build/dev1/rel/rc_example/data* ; rm -rf _build/dev2/rel/rc_example/data* ; rm -rf _build/dev3/rel/rc_example/data*
