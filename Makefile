
.PHONY: ct
all: compile eunit xref dialyze edoc

compile:
	@./rebar3 as dev compile

xref:
	@./rebar3 as test xref

clean:
	@./rebar3 clean

ct:
	@./rebar3 as test ct

cover:
	@./rebar3 as test cover

eunit:
	@./rebar3 as test eunit

edoc:
	@./rebar3 as dev edoc

start:
	@./rebar3 as dev shell

dialyze:
	@./rebar3 as test dialyzer
