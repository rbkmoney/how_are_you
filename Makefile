REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)

.PHONY: all

all: update compile xref dialyze clean distclean

update:
	$(REBAR) update

compile: update
	$(REBAR) compile

xref:
	$(REBAR) xref

dialyze:
	$(REBAR) as test dialyzer

clean:
	$(REBAR) clean

distclean:
	rm -rf _build

run:
	$(REBAR) as run shell
#test:
#	$(REBAR) ct
