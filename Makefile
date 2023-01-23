REBAR=$(shell which rebar3)

.PHONY: test
all: compile sample-restore test-bash edoc

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref

clean:
	@$(REBAR) clean
	@rm -fr sample/spam_*/{.git,_build}

test-bash: test
	bash test/tests.bash

test:
	@$(REBAR) test

edoc:
	@$(REBAR) edoc

start:
	@$(REBAR) shell

dialyze:
	@$(REBAR) dialyzer

sample-archive:
	@rm -fr sample/spam_*/{_build,.git}
	tar -zcvf sample/spam.tar.gz sample/spam_*

sample-restore:
	@tar -zxf sample/spam.tar.gz && echo "sample/spam_* restored"
