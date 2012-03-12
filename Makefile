REBAR = rebar
DIALYZER = dialyzer
RM = rm

.PHONY: all clean deps compile test ct build-plt dialyze update

all:	clean compile ct

clean:
	@$(REBAR) clean

squeaky: clean
	@$(REBAR) delete-deps

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

test:
	@$(REBAR) skip_deps=true eunit

ct:
	@$(REBAR) ct skip_deps=true

update:
	@$(REBAR) update-deps


build-plt:
	@$(DIALYZER) --build_plt --output_plt .gm_dialyzer.plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze:
	@$(DIALYZER) --src src --plt .gm_dialyzer.plt -Werror_handling \
		-Wrace_conditions -Wunmatched_returns -Wunderspecs -Wno_behaviours
