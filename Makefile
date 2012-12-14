PATH = /usr/bin/i86:/usr/bin:/bin

default: rel

get-deps:
	./rebar get-deps

static/nitrogen:
	cp -r deps/nitrogen_core/www static/nitrogen

compile: get-deps static/nitrogen
	./rebar compile

stop:
	-./rel/wfd/bin/wfd stop

start:
	-./rel/wfd/bin/wfd start

console:
	-./rel/wfd/bin/wfd console

attach:
	-./rel/wfd/bin/wfd attach

rel: compile stop
	-rm -rf rel/wfd
	cd rel; ../rebar generate

clean:
	-rm -rf static/nitrogen
	./rebar clean

distclean: clean
	./rebar delete-deps
	-rm -rf deps ebin rel/wfd*
