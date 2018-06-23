
.PHONY: test
all: compile test xref dialyze edoc

compile:
	@./rebar3 as dev compile

xref:
	@./rebar3 as test xref

clean:
	@./rebar3 clean

test:
	bash test/tests.bash

edoc:
	@./rebar3 as dev edoc

start:
	@./rebar3 as dev shell

dialyze:
	@./rebar3 as test dialyzer
