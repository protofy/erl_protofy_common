REBAR=`which rebar3`
all: compile docs
update: get-deps update-deps
full: clean get-deps update-deps compile docs tests
ci: get-deps update-deps compile docs tests
prod: get-deps update-deps compile-prod test docs

get-deps:
	@$(REBAR) get-deps
update-deps:
	@$(REBAR) upgrade --all
compile:
	@$(REBAR) compile
compile-prod:
	@$(REBAR) compile 
tests:
	@$(REBAR) eunit ct
clean:
	@$(REBAR) clean
clean-all:
	@$(REBAR) clean -a
docs:
	@$(REBAR) edoc
