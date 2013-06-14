PATH = /usr/bin/i86:/usr/bin:/bin

default: compile

get-deps:
	./rebar get-deps

static/nitrogen:
	cp -r deps/nitrogen_core/www static/nitrogen

compile: get-deps static/nitrogen
	./rebar compile

test: compile
	./rebar skip_deps=true ct

run: compile
	-mkdir log
	ERL_LIBS=deps:$(shell readlink -f ..) erl -sname wfd -boot start_sasl -config app.config -run wfd_app

rel: compile
	cd rel; ../rebar generate

clean:
	-rm -rf static/nitrogen
	./rebar clean

distclean: clean
	./rebar delete-deps
	-rm -rf deps ebin rel/wfd* log mnesia
