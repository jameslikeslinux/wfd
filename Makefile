default: compile

get-deps:
	./rebar get-deps

static/nitrogen:
	cp -r deps/nitrogen_core/www static/nitrogen

compile: get-deps static/nitrogen
	./rebar compile

test-app: compile
	./rebar skip_deps=true ct suites=app

test-web: compile
	bundle install
	./rebar skip_deps=true ct suites=web

test: compile
	bundle install
	./rebar skip_deps=true ct

run: compile
	ERL_LIBS=deps:$(shell readlink -f ..) erl -sname wfd -boot start_sasl -config app.config -run wfd_app

rel: compile
	cd rel; ../rebar generate

clean:
	./rebar clean
	-rm -rf static/nitrogen test/*.beam

distclean: clean
	./rebar delete-deps
	-rm -rf deps ebin rel/wfd* log mnesia test/logs
