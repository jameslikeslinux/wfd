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
	-mkdir logs
	ERL_LIBS=deps:$(shell readlink -f ..) ./deps/yaws/bin/yaws -i --sname wfd --mnesiadir mnesia --runmod wfd_app --conf rel/files/yaws.conf

rel: compile
	cd rel; ../rebar generate

clean:
	-rm -rf static/nitrogen
	./rebar clean

distclean: clean
	./rebar delete-deps
	-rm -rf deps ebin rel/wfd* logs mnesia
